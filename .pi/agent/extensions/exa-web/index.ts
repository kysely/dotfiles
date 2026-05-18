import { mkdtemp, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { StringEnum, Type } from "@mariozechner/pi-ai";
import {
	DEFAULT_MAX_BYTES,
	DEFAULT_MAX_LINES,
	formatSize,
	truncateHead,
	withFileMutationQueue,
	type ExtensionAPI,
	type TruncationResult,
} from "@mariozechner/pi-coding-agent";

const SearchWebParams = Type.Object({
	query: Type.String({ description: "Web search query." }),
	numResults: Type.Optional(
		Type.Integer({ minimum: 1, maximum: 20, description: "Number of search results to return. Defaults to 5, max 20." }),
	),
	type: Type.Optional(
		StringEnum(["auto", "neural", "keyword"] as const, {
			description: "Exa search type. Use auto by default; keyword for exact lexical searches; neural for semantic searches.",
		}),
	),
	useAutoprompt: Type.Optional(Type.Boolean({ description: "Whether Exa should rewrite the query. Defaults to true." })),
	includeDomains: Type.Optional(Type.Array(Type.String(), { description: "Restrict results to these domains." })),
	excludeDomains: Type.Optional(Type.Array(Type.String(), { description: "Exclude results from these domains." })),
	startPublishedDate: Type.Optional(Type.String({ description: "Only include pages published on or after this ISO date/time." })),
	endPublishedDate: Type.Optional(Type.String({ description: "Only include pages published on or before this ISO date/time." })),
	includeText: Type.Optional(Type.Boolean({ description: "Include extracted page text in each result. Defaults to false; use read-webpage for full page text." })),
	textMaxCharacters: Type.Optional(
		Type.Integer({ minimum: 200, maximum: 20000, description: "Max extracted text characters per result when includeText is true. Defaults to 2000." }),
	),
});

const ReadWebpageParams = Type.Object({
	url: Type.String({ description: "URL (or Exa result ID) to read using Exa's contents API." }),
	maxCharacters: Type.Optional(
		Type.Integer({ minimum: 500, maximum: 50000, description: "Max extracted text characters to return. Defaults to 12000." }),
	),
	livecrawl: Type.Optional(
		StringEnum(["never", "fallback", "always"] as const, {
			description: "Exa livecrawl mode. Defaults to fallback when supported by the API.",
		}),
	),
});

type JsonObject = Record<string, any>;

class ExaHttpError extends Error {
	constructor(
		message: string,
		readonly status: number,
		readonly bodyText: string,
		readonly bodyJson: unknown,
	) {
		super(message);
		this.name = "ExaHttpError";
	}
}

function exaBaseUrl() {
	return (process.env.EXA_BASE_URL || "https://api.exa.ai").replace(/\/+$/, "");
}

function exaApiKey() {
	const apiKey = process.env.EXA_API_KEY;
	if (!apiKey) {
		throw new Error("EXA_API_KEY is not set. Export it before using search-web or read-webpage.");
	}
	return apiKey;
}

function clampInteger(value: unknown, fallback: number, min: number, max: number) {
	const number = typeof value === "number" && Number.isFinite(value) ? Math.trunc(value) : fallback;
	return Math.min(max, Math.max(min, number));
}

function compactObject<T extends JsonObject>(object: T): T {
	const compacted: JsonObject = {};
	for (const [key, value] of Object.entries(object)) {
		if (value === undefined) continue;
		if (Array.isArray(value) && value.length === 0) continue;
		compacted[key] = value;
	}
	return compacted as T;
}

function errorMessage(status: number, bodyText: string, bodyJson: unknown) {
	if (bodyJson && typeof bodyJson === "object") {
		const body = bodyJson as JsonObject;
		const candidate = body.error || body.message || body.detail || body.details;
		if (typeof candidate === "string") return candidate;
		if (candidate) return JSON.stringify(candidate);
	}
	return bodyText.trim() || `HTTP ${status}`;
}

async function exaRequest(endpoint: string, body: JsonObject, signal?: AbortSignal) {
	const response = await fetch(`${exaBaseUrl()}${endpoint}`, {
		method: "POST",
		headers: {
			"content-type": "application/json",
			"x-api-key": exaApiKey(),
		},
		body: JSON.stringify(body),
		signal,
	});

	const bodyText = await response.text();
	let bodyJson: unknown;
	try {
		bodyJson = bodyText ? JSON.parse(bodyText) : undefined;
	} catch {
		bodyJson = undefined;
	}

	if (!response.ok) {
		const message = errorMessage(response.status, bodyText, bodyJson);
		throw new ExaHttpError(`Exa API ${response.status}: ${message}`, response.status, bodyText, bodyJson);
	}

	return (bodyJson ?? {}) as JsonObject;
}

async function exaRequestWithPayloadFallback(endpoint: string, payloads: JsonObject[], signal?: AbortSignal) {
	let lastError: unknown;
	for (const payload of payloads) {
		try {
			return await exaRequest(endpoint, payload, signal);
		} catch (error) {
			lastError = error;
			if (!(error instanceof ExaHttpError)) throw error;
			// Retry alternate Exa payload shapes only for validation-style errors.
			if (![400, 404, 422].includes(error.status)) throw error;
		}
	}
	throw lastError;
}

function searchPayloads(params: any) {
	const numResults = clampInteger(params.numResults, 5, 1, 20);
	const textMaxCharacters = clampInteger(params.textMaxCharacters, 2000, 200, 20000);
	const includeText = params.includeText === true;
	const base = compactObject({
		query: params.query,
		type: params.type ?? "auto",
		useAutoprompt: params.useAutoprompt ?? true,
		numResults,
		includeDomains: params.includeDomains,
		excludeDomains: params.excludeDomains,
		startPublishedDate: params.startPublishedDate,
		endPublishedDate: params.endPublishedDate,
	});

	if (!includeText) return [base];

	return [
		{ ...base, contents: { text: { maxCharacters: textMaxCharacters } } },
		{ ...base, text: { maxCharacters: textMaxCharacters } },
		{ ...base, text: true },
	];
}

function contentsPayloads(params: any) {
	const maxCharacters = clampInteger(params.maxCharacters, 12000, 500, 50000);
	const livecrawl = params.livecrawl ?? "fallback";
	return [
		compactObject({ ids: [params.url], text: { maxCharacters }, livecrawl }),
		compactObject({ urls: [params.url], text: { maxCharacters }, livecrawl }),
		compactObject({ ids: [params.url], contents: { text: { maxCharacters } }, livecrawl }),
		compactObject({ urls: [params.url], contents: { text: { maxCharacters } }, livecrawl }),
		compactObject({ ids: [params.url], text: true, livecrawl }),
	];
}

function resultArray(response: JsonObject) {
	if (Array.isArray(response.results)) return response.results as JsonObject[];
	if (Array.isArray(response.contents)) return response.contents as JsonObject[];
	if (Array.isArray(response.data)) return response.data as JsonObject[];
	return [];
}

function cleanText(value: unknown) {
	if (typeof value !== "string") return undefined;
	const text = value.trim();
	return text.length > 0 ? text : undefined;
}

function resultText(result: JsonObject) {
	return cleanText(result.text) || cleanText(result.content) || cleanText(result.extract) || cleanText(result.markdown);
}

function formatResultMetadata(result: JsonObject) {
	const lines: string[] = [];
	if (result.url) lines.push(`URL: ${result.url}`);
	if (result.id && result.id !== result.url) lines.push(`ID: ${result.id}`);
	if (result.publishedDate) lines.push(`Published: ${result.publishedDate}`);
	if (result.author) lines.push(`Author: ${result.author}`);
	if (typeof result.score === "number") lines.push(`Score: ${result.score}`);
	return lines;
}

function formatSearchResponse(query: string, response: JsonObject) {
	const results = resultArray(response);
	const lines: string[] = [`Exa search results for: ${query}`];
	if (typeof response.autopromptString === "string") lines.push(`Autoprompt: ${response.autopromptString}`);
	if (results.length === 0) {
		lines.push("", "No results.");
		return lines.join("\n");
	}

	results.forEach((result, index) => {
		const title = cleanText(result.title) || "(untitled)";
		lines.push("", `## ${index + 1}. ${title}`);
		lines.push(...formatResultMetadata(result));

		const summary = cleanText(result.summary);
		if (summary) lines.push("", "Summary:", summary);

		if (Array.isArray(result.highlights) && result.highlights.length > 0) {
			lines.push("", "Highlights:");
			for (const highlight of result.highlights.slice(0, 5)) {
				if (typeof highlight === "string") lines.push(`- ${highlight.trim()}`);
				else if (highlight?.text) lines.push(`- ${String(highlight.text).trim()}`);
			}
		}

		const text = resultText(result);
		if (text) lines.push("", "Text:", text);
	});

	return lines.join("\n");
}

function formatPageResponse(url: string, response: JsonObject) {
	const results = resultArray(response);
	const page = results[0] ?? response;
	const title = cleanText(page.title) || "(untitled)";
	const text = resultText(page);

	if (!text) {
		return `No extracted text was returned for ${url}.\n\nRaw Exa response:\n${JSON.stringify(response, null, 2)}`;
	}

	const lines = [`# ${title}`, ...formatResultMetadata({ ...page, url: page.url ?? url }), "", text];
	return lines.join("\n");
}

async function finalizeOutput(prefix: string, text: string, details: JsonObject) {
	const truncation = truncateHead(text, {
		maxLines: DEFAULT_MAX_LINES,
		maxBytes: DEFAULT_MAX_BYTES,
	});

	let output = truncation.content;
	if (truncation.truncated) {
		const tempDir = await mkdtemp(join(tmpdir(), "pi-exa-"));
		const tempFile = join(tempDir, `${prefix}.txt`);
		await withFileMutationQueue(tempFile, async () => {
			await writeFile(tempFile, text, "utf8");
		});

		details.truncation = truncation as TruncationResult;
		details.fullOutputPath = tempFile;
		output += `\n\n[Output truncated: showing ${truncation.outputLines} of ${truncation.totalLines} lines`;
		output += ` (${formatSize(truncation.outputBytes)} of ${formatSize(truncation.totalBytes)}).`;
		output += ` Full output saved to: ${tempFile}]`;
	}

	return {
		content: [{ type: "text" as const, text: output }],
		details,
	};
}

export default function (pi: ExtensionAPI) {
	pi.registerTool({
		name: "search-web",
		label: "Search Web",
		description: `Search the web using the Exa API. Requires EXA_API_KEY. Results are truncated to ${DEFAULT_MAX_LINES} lines or ${formatSize(DEFAULT_MAX_BYTES)} (whichever is hit first). Use read-webpage to fetch full text for a result URL.`,
		promptSnippet: "Search the web with Exa for current or external information.",
		promptGuidelines: [
			"Use search-web when the user asks for current, external, or web-based information that is not available in the local workspace.",
			"Use read-webpage after search-web when a result's page contents are needed in detail.",
		],
		parameters: SearchWebParams,
		async execute(_toolCallId, params, signal) {
			const response = await exaRequestWithPayloadFallback("/search", searchPayloads(params), signal);
			const output = formatSearchResponse(params.query, response);
			const results = resultArray(response);
			return finalizeOutput("search-web", output, {
				query: params.query,
				resultCount: results.length,
				results: results.map((result) => ({
					id: result.id,
					title: result.title,
					url: result.url,
					publishedDate: result.publishedDate,
					author: result.author,
					score: result.score,
				})),
			});
		},
	});

	pi.registerTool({
		name: "read-webpage",
		label: "Read Webpage",
		description: `Read/extract a webpage by URL or Exa result ID using the Exa contents API. Requires EXA_API_KEY. Output is truncated to ${DEFAULT_MAX_LINES} lines or ${formatSize(DEFAULT_MAX_BYTES)} (whichever is hit first).`,
		promptSnippet: "Read a webpage's extracted text with Exa by URL or result ID.",
		promptGuidelines: [
			"Use read-webpage when the contents of a specific URL are needed; use search-web first when you need to discover relevant URLs.",
		],
		parameters: ReadWebpageParams,
		async execute(_toolCallId, params, signal) {
			const response = await exaRequestWithPayloadFallback("/contents", contentsPayloads(params), signal);
			const output = formatPageResponse(params.url, response);
			const first = resultArray(response)[0] ?? response;
			return finalizeOutput("read-webpage", output, {
				url: params.url,
				id: first.id,
				title: first.title,
				resolvedUrl: first.url,
				publishedDate: first.publishedDate,
			});
		},
	});
}
