#!/usr/bin/env node
import { execFileSync } from "node:child_process";
import { realpathSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { pathToFileURL } from "node:url";
import { performance } from "node:perf_hooks";

const piCli = realpathSync(execFileSync("which", ["pi"], { encoding: "utf8" }).trim());
const piRoot = resolve(dirname(piCli), "..");
const piPkg = `${piRoot}/dist/index.js`;
const piTui = `${piRoot}/node_modules/@earendil-works/pi-tui/dist/index.js`;
const extensionPath = new URL("../extensions/rdksl-ui.ts", import.meta.url).pathname;
const { createJiti } = await import(pathToFileURL(`${piRoot}/node_modules/jiti/lib/jiti.mjs`).href);

const mode = process.argv[2] ?? "extension"; // extension | baseline
const pairs = Number(process.argv[3] ?? 300);
const iterations = Number(process.argv[4] ?? 30);

const { TUI, Container } = await import(piTui);
const { initTheme, AssistantMessageComponent, UserMessageComponent, ToolExecutionComponent } = await import(piPkg);
initTheme("light", false);

if (mode !== "baseline") {
  const jiti = createJiti(import.meta.url, {
    alias: {
      "@earendil-works/pi-tui": piTui,
      "@earendil-works/pi-coding-agent": piPkg,
    },
  });
  const mod = await jiti.import(extensionPath);
  const theme = globalThis[Symbol.for("@earendil-works/pi-coding-agent:theme")];
  mod.default({
    on(event, handler) {
      if (event === "session_start") handler({}, { ui: { theme } });
    },
  });
}

const terminal = {
  columns: 120,
  rows: 40,
  start() {},
  stop() {},
  write() {},
  moveBy() {},
  hideCursor() {},
  showCursor() {},
  clearLine() {},
  clearFromCursor() {},
  clearScreen() {},
};

const tui = new TUI(terminal);
const chat = new Container();
const usage = () => ({ input: 0, output: 0, cacheRead: 0, cacheWrite: 0, cost: { total: 0 } });

function assistantText(i) {
  return `Here is **message ${i}** with a [link](app/files) and inline \`code-${i}\`.\n\n## Heading ${i}\n\n- **Frontend/routing:** React 19 + Router 7 and \`app/routes\`\n- **Backend:** server functions and PostgreSQL\n- **Infra:** Docker Compose, Hetzner Cloud, TLS\n\n\`\`\`text\n/reload\nsome code-ish output ${i}\n\`\`\`\n\nFinal paragraph with more words to make wrapping and markdown parsing non-trivial.`;
}

for (let i = 0; i < pairs; i++) {
  chat.addChild(new UserMessageComponent(`Question ${i}: what changed?`));
  chat.addChild(new AssistantMessageComponent({
    role: "assistant",
    content: [{ type: "text", text: assistantText(i) }],
    stopReason: "end",
    usage: usage(),
  }));

  if (i % 5 === 0) {
    const command = "node <<'EOF'\nconsole.log('a')\nconsole.log('b')\nconsole.log('c')\nEOF";
    const tool = new ToolExecutionComponent("bash", `id-${i}`, { command }, {}, undefined, tui, process.cwd());
    tool.updateResult({
      content: [{ type: "text", text: "out1\nout2\nout3\nout4\nout5" }],
      details: {},
      isError: false,
    }, false);
    chat.addChild(tool);
  }
}

// Mirror Pi's current root child order:
// header, loaded resources, chat, pending messages, status,
// widgets-above, editor, widgets-below, footer.
tui.addChild({ render: () => [], invalidate() {} });
tui.addChild({ render: () => [], invalidate() {} });
tui.addChild(chat);
for (let i = 0; i < 6; i++) tui.addChild({ render: () => [], invalidate() {} });

const firstStart = performance.now();
tui.doRender();
const first = performance.now() - firstStart;

const samples = [];
for (let i = 0; i < iterations; i++) {
  const start = performance.now();
  tui.doRender();
  samples.push(performance.now() - start);
}

const avg = samples.reduce((sum, value) => sum + value, 0) / samples.length;

// Regression: Pi can render a streaming assistant row once while it is still
// empty, then update that same row when the first provider content arrives.
// The outer chat cache must not retain the initial empty rendering.
const streamingTui = new TUI(terminal);
const streamingChat = new Container();
const streamingAssistant = new AssistantMessageComponent(undefined);
streamingChat.addChild(streamingAssistant);
streamingTui.addChild({ render: () => [], invalidate() {} });
streamingTui.addChild({ render: () => [], invalidate() {} });
streamingTui.addChild(streamingChat);
for (let i = 0; i < 6; i++) streamingTui.addChild({ render: () => [], invalidate() {} });
streamingTui.doRender();
streamingAssistant.updateContent({
  role: "assistant",
  content: [{ type: "text", text: "STREAMED_SENTINEL" }],
  stopReason: "end",
  usage: usage(),
});
streamingTui.doRender();
const emptyStreamingUpdateVisible = streamingTui.previousLines.some((line) => line.includes("STREAMED_SENTINEL"));
if (!emptyStreamingUpdateVisible) {
  throw new Error("Streaming assistant content remained hidden after its initial empty render");
}

console.log(JSON.stringify({
  mode,
  pairs,
  iterations,
  firstMs: Number(first.toFixed(3)),
  avgNoopMs: Number(avg.toFixed(3)),
  minNoopMs: Number(Math.min(...samples).toFixed(3)),
  maxNoopMs: Number(Math.max(...samples).toFixed(3)),
  lines: tui.previousLines.length,
  emptyStreamingUpdateVisible,
}, null, 2));
