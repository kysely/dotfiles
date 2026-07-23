import { existsSync, readFileSync, statSync } from "node:fs";
import { isAbsolute, join, relative, resolve, sep } from "node:path";
import { AssistantMessageComponent, FooterComponent, Theme, ToolExecutionComponent, UserMessageComponent } from "@earendil-works/pi-coding-agent";
import { CURSOR_MARKER, Editor, getKeybindings, isKeyRelease, Markdown, matchesKey, TUI, truncateToWidth, visibleWidth } from "@earendil-works/pi-tui";

const PATCH_FLAG = Symbol.for("radek.pi.tui-mixed-horizontal-padding");
const EDITOR_PATCH_FLAG = Symbol.for("radek.pi.prompt-caret-editor");
const USER_MESSAGE_PATCH_FLAG = Symbol.for("radek.pi.user-message-caret");
const ASSISTANT_MESSAGE_PATCH_FLAG = Symbol.for("radek.pi.assistant-message-caret");
const TOOL_CALL_PATCH_FLAG = Symbol.for("radek.pi.tool-call-caret");
const FOOTER_PATCH_FLAG = Symbol.for("radek.pi.footer-one-line-status");
const FOOTER_DATA_PATCH_FLAG = Symbol.for("radek.pi.footer-data-status-version");
const MARKDOWN_PATCH_FLAG = Symbol.for("radek.pi.markdown-base-text-color");
const LEGACY_BACKGROUND_PATCH_FLAG = Symbol.for("radek.pi.subtle-backgrounds");
const PATCH_VERSION = 30;
const EDITOR_PATCH_VERSION = 6;
const USER_MESSAGE_PATCH_VERSION = 8;
const ASSISTANT_MESSAGE_PATCH_VERSION = 11;
const TOOL_CALL_PATCH_VERSION = 18;
const FOOTER_PATCH_VERSION = 16;
const MARKDOWN_PATCH_VERSION = 3;
const CONFIG_CHECK_INTERVAL_MS = 1000;
const THEME_CACHE_VERSION = 1;
const CHAT_ENTRY_BLOCK_CACHE_VERSION = 4;
const CHAT_CONTAINER_CACHE_VERSION = 5;

const PI_THEME_SYMBOL = Symbol.for("@earendil-works/pi-coding-agent:theme");

let currentTheme: any;
let nextChatEntryId = 1;

const trimCache = new WeakMap<string[], string[]>();
const TERMINAL_SEGMENT_RESET = "\x1b[0m\x1b]8;;\x07";
const THAI_LAO_AM_REGEX = /[\u0e33\u0eb3]/;
const THAI_LAO_AM_GLOBAL_REGEX = /[\u0e33\u0eb3]/g;
const MOUSE_REPORTING_ENABLE = "\x1b[?1000h\x1b[?1006h";
const MOUSE_REPORTING_DISABLE = "\x1b[?1006l\x1b[?1000l";
const MOUSE_WHEEL_SCROLL_LINES = 3;

type Padding = { left: number; right: number; contentWidth: number };

type ComponentLike = {
  render(width: number): string[];
  children?: ComponentLike[];
  __radekPromptCaret?: boolean;
  __radekFullWidthUserMessage?: boolean;
  __radekAssistantResponseCaret?: boolean;
  __radekToolCallCaret?: boolean;
  __radekChatBlockCache?: ChatEntryBlockCache;
  __radekChatBlockVersion?: number;
  __radekChatEntryId?: number;
  __radekChatContainerOwner?: ComponentLike;
  __radekChatContainerCache?: ChatContainerCache;
  __radekChatContainerVersion?: number;
};

type MaybeTui = {
  terminal?: { columns?: number; rows?: number; write?: (text: string) => void };
  children?: ComponentLike[];
  render(width: number): string[];
  handleInput?: (data: string, ...args: unknown[]) => unknown;
  start?: (...args: unknown[]) => unknown;
  stop?: (...args: unknown[]) => unknown;
  requestRender?: () => void;
  compositeOverlays?: (...args: unknown[]) => string[];
  getHorizontalPadding?: (width: number) => Padding;
  applyHorizontalPadding?: (lines: string[], padding: Padding) => string[];
  previousLines?: string[];
  previousWidth?: number;
  previousHeight?: number;
  previousViewportTop?: number;
  positionHardwareCursor?: (cursorPos: { row: number; col: number } | null, lineCount: number) => void;
  overlayStack?: unknown[];
  isOverlayVisible?: (entry: unknown) => boolean;
  focusedComponent?: unknown;
  cursorRow?: number;
  hardwareCursorRow?: number;
  maxLinesRendered?: number;
  __radekCurrentFrameMeta?: ChatFrameMeta;
  __radekPreviousFrameMeta?: ChatFrameMeta;
  __radekStickyPromptActive?: boolean;
  __radekStickyScrollOffset?: number;
  __radekStickyScrollMaxOffset?: number;
  __radekStickyScrollTotalLines?: number;
  __radekStickyScrollViewportHeight?: number;
  __radekStickyJumpOnTranscriptGrowth?: boolean;
  __radekMouseReportingEnabled?: boolean;
};

type PatchState = {
  version: number;
  originalDoRender?: unknown;
  originalHandleInput?: unknown;
  originalStart?: unknown;
  originalStop?: unknown;
};

type RenderPatchState = {
  version: number;
  originalRender?: unknown;
  originalUpdateContent?: unknown;
  originalUpdateDisplay?: unknown;
};

type LinesCache = {
  version: number;
  width: number;
  themeKey: string;
  lines: string[];
};

type ChatEntryBlockCache = {
  version: number;
  width: number;
  contentPadding: number;
  themeKey: string;
  mutationVersion: number;
  lines: string[];
};

type ChatBlockMeta = {
  key: string;
  start: number;
  length: number;
};

type ChatContainerCache = {
  version: number;
  width: number;
  contentPadding: number;
  baseThemeKey: string;
  containerVersion: number;
  childCount: number;
  lastChild?: ComponentLike;
  lines: string[];
  signature: string;
  blocks: ChatBlockMeta[];
};

type ChatContainerRenderResult = {
  lines: string[];
  signature?: string;
  blocks?: ChatBlockMeta[];
};

type ChatFrameMeta = {
  width: number;
  totalLines: number;
  chatStart: number;
  chatLineCount: number;
  chatSignature: string;
  chatBlocks?: ChatBlockMeta[];
};

type LegacyBackgroundPatchState = {
  version: number;
  originalBg?: (color: string, text: string) => string;
  originalFg?: (color: string, text: string) => string;
};

type FooterStatusMapPatchState = {
  version: number;
  owner: any;
  originalSet?: (...args: any[]) => any;
  originalDelete?: (...args: any[]) => any;
  originalClear?: (...args: any[]) => any;
};

type PrototypePatchState = Record<PropertyKey, unknown> & { version?: number };
type MethodRestoreSpec = [methodName: PropertyKey, originalKey: PropertyKey];

function asPrototypePatchState(value: unknown): PrototypePatchState | undefined {
  return value && typeof value === "object" ? value as PrototypePatchState : undefined;
}

function restorePrototypeMethods(target: Record<PropertyKey, unknown>, flag: PropertyKey, methods: MethodRestoreSpec[]): PrototypePatchState | undefined {
  const existing = asPrototypePatchState(target[flag]);
  if (!existing) return undefined;

  for (const [methodName, originalKey] of methods) {
    const original = existing[originalKey];
    if (typeof original === "function") {
      target[methodName] = original;
    }
  }

  return existing;
}

type CustomUiConfig = {
  padding: {
    content: number;
    prompt: number;
  };
  prompt: {
    caret: string;
  };
  assistant: {
    finalIcon: string;
  };
  tools: {
    compactSummaries: boolean;
    icons: Record<string, string>;
    bash: {
      commandPreviewLines: number;
      outputPreviewLines: number;
    };
  };
  reasoning: {
    offLabel: string;
    icons: Record<string, string>;
  };
};

const DEFAULT_CUSTOM_UI_CONFIG: CustomUiConfig = {
  padding: {
    content: 1,
    prompt: 2,
  },
  prompt: {
    caret: "❯",
  },
  assistant: {
    finalIcon: "⏺",
  },
  tools: {
    compactSummaries: true,
    icons: {
      read: "◎",
      write: "◉",
      edit: "◉",
      bash: "◐",
    },
    bash: {
      commandPreviewLines: 1,
      outputPreviewLines: 5,
    },
  },
  reasoning: {
    offLabel: "thinking off",
    icons: {
      minimal: "◌",
      low: "○",
      medium: "◐",
      high: "●",
      xhigh: "◉",
      custom: "◉",
      default: "◉",
    },
  },
};

let cachedConfig: CustomUiConfig | undefined;
let cachedConfigPath: string | undefined;
let cachedConfigMtimeMs = -1;
let cachedConfigKey = "default";
let nextConfigCheckAt = 0;

function isRecord(value: unknown): value is Record<string, unknown> {
  return Boolean(value) && typeof value === "object" && !Array.isArray(value);
}

function mergeConfig<T>(base: T, override: unknown): T {
  if (!isRecord(base) || !isRecord(override)) return base;
  const target = base as Record<string, unknown>;
  for (const [key, value] of Object.entries(override)) {
    if (isRecord(value) && isRecord(target[key])) {
      target[key] = mergeConfig(target[key], value);
    } else if (value !== undefined) {
      target[key] = value;
    }
  }
  return base;
}

function cloneDefaultConfig(): CustomUiConfig {
  return JSON.parse(JSON.stringify(DEFAULT_CUSTOM_UI_CONFIG)) as CustomUiConfig;
}

function configCandidates(): string[] {
  const paths: string[] = [];
  if (process.env.PI_RDKSL_UI_CONFIG) paths.push(resolve(process.env.PI_RDKSL_UI_CONFIG));

  const home = process.env.HOME || process.env.USERPROFILE;
  const agentDir = process.env.PI_CODING_AGENT_DIR || (home ? join(home, ".pi", "agent") : undefined);
  if (agentDir) paths.push(resolve(agentDir, "rdksl-ui.config.json"));
  return paths;
}

function getCustomUiConfig(): CustomUiConfig {
  const now = Date.now();
  if (cachedConfig && now < nextConfigCheckAt) return cachedConfig;
  nextConfigCheckAt = now + CONFIG_CHECK_INTERVAL_MS;

  const configPath = configCandidates().find((path) => existsSync(path));
  const mtimeMs = configPath ? statSync(configPath).mtimeMs : -1;
  if (cachedConfig && configPath === cachedConfigPath && mtimeMs === cachedConfigMtimeMs) {
    return cachedConfig;
  }

  let config = cloneDefaultConfig();
  if (configPath) {
    try {
      config = mergeConfig(config, JSON.parse(readFileSync(configPath, "utf8")));
    } catch (error) {
      console.error(`[rdksl-ui] Failed to read ${configPath}:`, error);
    }
  }

  cachedConfig = config;
  cachedConfigPath = configPath;
  cachedConfigMtimeMs = mtimeMs;
  cachedConfigKey = configPath ? `${configPath}:${mtimeMs}` : "default";
  return config;
}

function envString(names: string[], fallback: string): string {
  for (const name of names) {
    const raw = process.env[name];
    if (raw !== undefined) return raw;
  }
  return fallback;
}

function envBool(names: string[], fallback: boolean): boolean {
  for (const name of names) {
    const raw = process.env[name];
    if (raw === undefined) continue;
    if (/^(1|true|yes|on)$/i.test(raw)) return true;
    if (/^(0|false|no|off)$/i.test(raw)) return false;
  }
  return fallback;
}

function envPadding(names: string[], fallback: number): number {
  for (const name of names) {
    const raw = process.env[name];
    if (raw === undefined) continue;
    const value = Number.parseInt(raw, 10);
    return Number.isFinite(value) ? Math.max(0, value) : fallback;
  }
  return fallback;
}

function getEnvInt(names: string[], fallback: number, min = 0): number {
  return Math.max(min, envPadding(names, fallback));
}

function getContentPadding(): number {
  return getEnvInt(["PI_UI_CONTENT_PADDING", "PI_TUI_CONTENT_PADDING", "PI_TUI_HORIZONTAL_PADDING"], getCustomUiConfig().padding.content, 0);
}

function getBashCommandPreviewLines(): number {
  return getEnvInt(["PI_UI_BASH_COMMAND_PREVIEW_LINES", "PI_TUI_BASH_COMMAND_PREVIEW_LINES"], getCustomUiConfig().tools.bash.commandPreviewLines, 1);
}

function getBashOutputPreviewLines(): number {
  return getEnvInt(["PI_UI_BASH_OUTPUT_PREVIEW_LINES", "PI_UI_BASH_PREVIEW_LINES", "PI_TUI_BASH_PREVIEW_LINES"], getCustomUiConfig().tools.bash.outputPreviewLines, 0);
}

function getPromptPadding(): number {
  if (process.env.PI_UI_PROMPT_PADDING !== undefined || process.env.PI_TUI_PROMPT_PADDING !== undefined) {
    return getEnvInt(["PI_UI_PROMPT_PADDING", "PI_TUI_PROMPT_PADDING"], getCustomUiConfig().padding.prompt, 0);
  }

  // Preserve the old one-knob "disable padding" behavior.
  if (process.env.PI_TUI_HORIZONTAL_PADDING !== undefined) {
    const value = Number.parseInt(process.env.PI_TUI_HORIZONTAL_PADDING, 10);
    if (Number.isFinite(value) && value <= 0) return 0;
  }

  return getCustomUiConfig().padding.prompt;
}

function promptCaretGlyph(): string {
  return envString(["PI_UI_PROMPT_CARET", "PI_TUI_PROMPT_CARET"], getCustomUiConfig().prompt.caret).trimEnd() || DEFAULT_CUSTOM_UI_CONFIG.prompt.caret;
}

function promptCaretText(): string {
  return `${promptCaretGlyph()} `;
}

function assistantFinalIcon(): string {
  return envString(["PI_UI_ASSISTANT_ICON", "PI_UI_ASSISTANT_FINAL_ICON"], getCustomUiConfig().assistant.finalIcon).trimEnd() || DEFAULT_CUSTOM_UI_CONFIG.assistant.finalIcon;
}

function configuredToolIcon(toolName: string | undefined): string | undefined {
  if (!toolName) return undefined;
  const envKey = `PI_UI_TOOL_ICON_${toolName.toUpperCase().replace(/[^A-Z0-9]/g, "_")}`;
  return envString([envKey], getCustomUiConfig().tools.icons[toolName] ?? "") || undefined;
}

function toolSummariesEnabled(): boolean {
  return envBool(["PI_UI_TOOL_SUMMARIES", "PI_UI_COMPACT_TOOL_SUMMARIES"], getCustomUiConfig().tools.compactSummaries);
}

function configuredReasoningIcon(level: string): string {
  const envKey = `PI_UI_REASONING_ICON_${level.toUpperCase().replace(/[^A-Z0-9]/g, "_")}`;
  const icons = getCustomUiConfig().reasoning.icons;
  return envString([envKey], icons[level] ?? icons.default ?? DEFAULT_CUSTOM_UI_CONFIG.reasoning.icons.default);
}

function configuredReasoningOffLabel(): string {
  return envString(["PI_UI_REASONING_OFF_LABEL"], getCustomUiConfig().reasoning.offLabel);
}

function getPadding(width: number, requested: number): Padding {
  if (requested === 0 || width <= 1) {
    return { left: 0, right: 0, contentWidth: Math.max(1, width) };
  }

  // Keep at least one column for content on very narrow terminals.
  const maxTotalPadding = Math.max(0, width - 1);
  const left = Math.min(requested, Math.ceil(maxTotalPadding / 2));
  const right = Math.min(requested, maxTotalPadding - left);
  return { left, right, contentWidth: Math.max(1, width - left - right) };
}

function splitLeadingPromptMarkers(line: string): { prefix: string; rest: string } {
  let index = 0;

  // Keep OSC 133 prompt-zone markers at the absolute start of the line.
  // Some terminals render those zones strangely if visible padding comes first.
  while (line.startsWith("\x1b]133;", index)) {
    const end = line.indexOf("\x07", index);
    if (end === -1) break;
    index = end + 1;
  }

  return { prefix: line.slice(0, index), rest: line.slice(index) };
}

function addLeftPadding(lines: string[], padding: Padding): string[] {
  if (padding.left === 0 && padding.right === 0) return lines;
  const leftMargin = " ".repeat(padding.left);

  // Leave blank reserved rows blank (important for multi-row inline images).
  // For prompt markers, insert visual padding after the zero-width marker.
  return lines.map((line) => {
    if (line === "") return line;
    const { prefix, rest } = splitLeadingPromptMarkers(line);
    return prefix + leftMargin + rest;
  });
}

function renderChildWithPadding(child: ComponentLike, terminalWidth: number, requestedPadding: number): string[] {
  const padding = getPadding(terminalWidth, requestedPadding);
  return addLeftPadding(child.render(padding.contentWidth), padding);
}

function setPromptCaretMode(component: ComponentLike, enabled: boolean) {
  component.__radekPromptCaret = enabled;
  for (const child of component.children ?? []) {
    setPromptCaretMode(child, enabled);
  }
}

function renderPromptEditorChild(child: ComponentLike, terminalWidth: number): string[] {
  setPromptCaretMode(child, true);
  try {
    return child.render(terminalWidth);
  } finally {
    setPromptCaretMode(child, false);
  }
}

function isChatContainerChild(index: number, childCount: number): boolean {
  // Pi's root TUI children are currently appended as:
  // header, loaded-resources, chat, pending-messages, status,
  // widgets-above, editor, widgets-below, footer.
  // Older/synthetic layouts omit loaded-resources/status variants, so derive the
  // chat slot from the stable seven-child tail when possible and fall back to 1.
  const expectedIndex = childCount >= 8 ? childCount - 7 : 1;
  return index === expectedIndex;
}

function isEditorContainerChild(index: number, childCount: number): boolean {
  return index === childCount - 3;
}

function isPromptAreaChild(index: number, childCount: number): boolean {
  // The final prompt stack is widgets-above, editor, widgets-below, footer.
  return index >= Math.max(0, childCount - 4);
}

function extendBorderLine(line: string, width: number, color: (text: string) => string): string {
  const missing = Math.max(0, width - visibleWidth(line));
  return missing === 0 ? line : line + color("─".repeat(missing));
}

function restoreLegacyThemePatch(target: any) {
  const existing = target?.[LEGACY_BACKGROUND_PATCH_FLAG] as LegacyBackgroundPatchState | undefined;
  if (!existing) return;
  if (existing.originalBg) target.bg = existing.originalBg;
  if (existing.originalFg) target.fg = existing.originalFg;
  try {
    delete target[LEGACY_BACKGROUND_PATCH_FLAG];
  } catch {
    target[LEGACY_BACKGROUND_PATCH_FLAG] = undefined;
  }
}

function restoreLegacyThemeMonkeyPatches(theme?: any) {
  restoreLegacyThemePatch((Theme as any)?.prototype);
  restoreLegacyThemePatch(theme);
}

function activeTheme(): any {
  return (globalThis as any)[PI_THEME_SYMBOL] ?? currentTheme;
}

function themeCacheKey(): string {
  getCustomUiConfig();
  const theme = activeTheme();
  const mode = typeof theme?.getColorMode === "function" ? theme.getColorMode() : "unknown";
  return `${THEME_CACHE_VERSION}:${cachedConfigKey}:${theme?.name ?? "unknown"}:${mode}`;
}

function toolCacheKey(toolName: string | undefined, base = themeCacheKey()): string {
  return toolName === "bash"
    ? `${base}:bash-command-lines=${getBashCommandPreviewLines()}:bash-output-lines=${getBashOutputPreviewLines()}`
    : base;
}

function patchMarkdownBaseTextColor() {
  const proto = Markdown.prototype as unknown as Record<PropertyKey, unknown>;
  const existing = proto[MARKDOWN_PATCH_FLAG] as RenderPatchState | undefined;
  if (existing?.version === MARKDOWN_PATCH_VERSION && typeof existing.originalRender === "function") return;

  restorePrototypeMethods(proto, MARKDOWN_PATCH_FLAG, [["render", "originalRender"]]);
  const originalRender = proto.render;
  if (typeof originalRender !== "function") return;

  proto.render = function renderMarkdownWithBaseTextColor(this: any, width: number): string[] {
    const cacheKey = themeCacheKey();
    if (this.__radekMarkdownThemeKey !== cacheKey) {
      this.__radekMarkdownThemeKey = cacheKey;
      this.invalidate?.();
    }

    if (this.defaultTextStyle) {
      return originalRender.call(this, width);
    }

    const originalDefaultTextStyle = this.defaultTextStyle;
    const originalDefaultStylePrefix = this.defaultStylePrefix;
    this.defaultTextStyle = { color: (text: string) => activeTheme()?.fg ? activeTheme().fg("text", text) : text };
    this.defaultStylePrefix = undefined;
    try {
      return originalRender.call(this, width);
    } finally {
      this.defaultTextStyle = originalDefaultTextStyle;
      this.defaultStylePrefix = originalDefaultStylePrefix;
    }
  };

  proto[MARKDOWN_PATCH_FLAG] = { version: MARKDOWN_PATCH_VERSION, originalRender };
}

function fg(color: string, text: string): string {
  const theme = activeTheme();
  return theme?.fg ? theme.fg(color, text) : text;
}

function dim(text: string): string {
  return fg("dim", text);
}

function mutedCaret(): string {
  return `\x1b[2m${fg("dim", promptCaretGlyph())}\x1b[22m `;
}

function stripPromptFakeCursor(line: string): string {
  // Pi normally renders a fake inverse-video cursor. For a real blink, keep the
  // cursor marker but remove the fake styling so Ghostty's hardware cursor can
  // blink over normal text/space.
  return line.replace(/\x1b\[(?:5;)?7m([^\x1b]*)\x1b\[0m/g, "$1");
}

function patchEditorRender() {
  const proto = Editor.prototype as unknown as Record<PropertyKey, unknown>;
  const existing = proto[EDITOR_PATCH_FLAG] as RenderPatchState | boolean | undefined;
  if (typeof existing === "object" && existing.version === EDITOR_PATCH_VERSION && typeof existing.originalRender === "function") return;

  if (existing !== undefined && (typeof existing !== "object" || typeof existing.originalRender !== "function") && typeof proto.render === "function") {
    // The pre-hardening Editor patch did not keep originalRender, so it cannot
    // be unwrapped safely in a live Pi process. Adopt that legacy wrapper as the
    // restore target instead of stacking another wrapper during this /reload.
    proto[EDITOR_PATCH_FLAG] = { version: EDITOR_PATCH_VERSION, originalRender: proto.render };
    return;
  }

  restorePrototypeMethods(proto, EDITOR_PATCH_FLAG, [["render", "originalRender"]]);
  const originalRender = proto.render;
  if (typeof originalRender !== "function") return;

  proto.render = function renderPromptWithCaret(this: any, width: number): string[] {
    if (!this.__radekPromptCaret) {
      return (originalRender as Function).call(this, width);
    }

    const promptCaret = promptCaretText();
    const caretWidth = visibleWidth(promptCaret);
    if (width <= caretWidth + 1) {
      return (originalRender as Function).call(this, width);
    }

    const innerWidth = Math.max(1, width - caretWidth);

    // The caret supplies the prompt gutter. Suppress Editor's own horizontal
    // padding so the textarea text starts exactly after "❯ ". If a legacy
    // extension wrapper is already around Editor.render and cannot be unwrapped
    // because it did not store its original, turn prompt-caret mode off while
    // asking it for the base editor lines. New versions store originalRender so
    // future /reloads restore before rewrapping instead of stacking.
    const originalPaddingX = this.paddingX;
    const originalPromptCaret = this.__radekPromptCaret;
    this.paddingX = 0;
    this.__radekPromptCaret = false;
    let lines: string[];
    try {
      lines = (originalRender as Function).call(this, innerWidth) as string[];
    } finally {
      this.paddingX = originalPaddingX;
      this.__radekPromptCaret = originalPromptCaret;
    }
    if (lines.length === 0) return lines;

    const autocompleteWidth = innerWidth;
    const autocompleteCount = this.autocompleteState && this.autocompleteList
      ? this.autocompleteList.render(autocompleteWidth).length
      : 0;
    const bottomBorderIndex = Math.max(0, lines.length - autocompleteCount - 1);
    const borderColor = typeof this.borderColor === "function" ? this.borderColor.bind(this) : (text: string) => text;
    const caret = borderColor(promptCaret.trimEnd()) + " ";

    return lines.map((line, index) => {
      if (index === 0 || index === bottomBorderIndex) {
        return extendBorderLine(line, width, borderColor);
      }
      if (index < bottomBorderIndex) {
        // Show the prompt caret only once, on the first textarea row.
        return (index === 1 ? caret : " ".repeat(caretWidth)) + stripPromptFakeCursor(line);
      }
      // Autocomplete rows align with the textarea text, not the caret.
      return " ".repeat(caretWidth) + stripPromptFakeCursor(line);
    });
  };

  proto[EDITOR_PATCH_FLAG] = { version: EDITOR_PATCH_VERSION, originalRender };
}

function patchUserMessageRender() {
  const proto = UserMessageComponent.prototype as unknown as Record<PropertyKey, unknown>;
  const existing = proto[USER_MESSAGE_PATCH_FLAG] as RenderPatchState | undefined;
  if (existing?.version === USER_MESSAGE_PATCH_VERSION && typeof existing.originalRender === "function") return;

  restorePrototypeMethods(proto, USER_MESSAGE_PATCH_FLAG, [["render", "originalRender"]]);
  const originalRender = proto.render;
  if (typeof originalRender !== "function") return;

  proto.render = function renderUserMessageWithCaret(this: any, width: number): string[] {
    if (!this.__radekFullWidthUserMessage) {
      return (originalRender as Function).call(this, width);
    }

    const cacheKey = themeCacheKey();
    const cache = this.__radekUserRenderCache as LinesCache | undefined;
    if (cache?.version === USER_MESSAGE_PATCH_VERSION && cache.width === width && cache.themeKey === cacheKey) {
      return cache.lines;
    }

    const caretWidth = visibleWidth(promptCaretText());
    const contentBox = this.contentBox ?? this.children?.[0];
    if (width <= caretWidth + 1 || !contentBox?.children?.length || typeof contentBox.applyBg !== "function") {
      const originalFlag = this.__radekFullWidthUserMessage;
      this.__radekFullWidthUserMessage = false;
      try {
        return (originalRender as Function).call(this, width);
      } finally {
        this.__radekFullWidthUserMessage = originalFlag;
      }
    }

    const contentWidth = Math.max(1, width - caretWidth);
    const markdown = contentBox.children[0];
    const contentLines = markdown.render(contentWidth) as string[];
    const caret = mutedCaret();
    const gutter = " ".repeat(caretWidth);
    const lines = contentLines.map((line, index) => contentBox.applyBg((index === 0 ? caret : gutter) + line, width));

    if (lines.length > 0) {
      lines[0] = "\x1b]133;A\x07" + lines[0];
      lines[lines.length - 1] = "\x1b]133;B\x07\x1b]133;C\x07" + lines[lines.length - 1];
    }
    this.__radekUserRenderCache = { version: USER_MESSAGE_PATCH_VERSION, width, themeKey: cacheKey, lines };
    return lines;
  };

  proto[USER_MESSAGE_PATCH_FLAG] = { version: USER_MESSAGE_PATCH_VERSION, originalRender };
}

function assistantCaret(): string {
  return fg("text", assistantFinalIcon()) + " ";
}

function getFirstAssistantTextChildIndex(component: any): number | undefined {
  const message = component.lastMessage;
  const children = component.contentContainer?.children;
  if (!message || !Array.isArray(message.content) || !Array.isArray(children)) return undefined;

  const hasVisibleContent = message.content.some((content: any) =>
    (content.type === "text" && content.text?.trim()) || (content.type === "thinking" && content.thinking?.trim()),
  );
  let childIndex = hasVisibleContent ? 1 : 0; // initial Spacer(1), when present

  for (let i = 0; i < message.content.length; i++) {
    const content = message.content[i];
    if (content.type === "text" && content.text?.trim()) {
      return childIndex;
    }
    if (content.type === "thinking" && content.thinking?.trim()) {
      childIndex += 1;
      const hasVisibleContentAfter = message.content
        .slice(i + 1)
        .some((next: any) => (next.type === "text" && next.text?.trim()) || (next.type === "thinking" && next.thinking?.trim()));
      if (hasVisibleContentAfter) childIndex += 1; // Spacer inserted by AssistantMessageComponent
    }
  }

  return undefined;
}

function removeMarkdownLeftMargin(line: string, targetWidth: number): string {
  const stripped = line.startsWith(" ") ? line.slice(1) : line;
  return stripped + " ".repeat(Math.max(0, targetWidth - visibleWidth(stripped)));
}

function stripAnsiForTest(text: string): string {
  return text
    .replace(/\x1b\[[0-?]*[ -/]*[@-~]/g, "")
    .replace(/\x1b\][^\x07]*(?:\x07|\x1b\\)/g, "")
    .replace(/\x1b_[^\x07]*(?:\x07|\x1b\\)/g, "");
}

function splitLeadingAnsi(line: string): { prefix: string; rest: string } {
  let index = 0;
  while (index < line.length) {
    const csi = line.slice(index).match(/^\x1b\[[0-?]*[ -/]*[@-~]/);
    if (csi) {
      index += csi[0].length;
      continue;
    }
    const osc = line.slice(index).match(/^\x1b\][^\x07]*(?:\x07|\x1b\\)/);
    if (osc) {
      index += osc[0].length;
      continue;
    }
    const apc = line.slice(index).match(/^\x1b_[^\x07]*(?:\x07|\x1b\\)/);
    if (apc) {
      index += apc[0].length;
      continue;
    }
    break;
  }
  return { prefix: line.slice(0, index), rest: line.slice(index) };
}

function toolIcon(toolName: string | undefined): string | undefined {
  return configuredToolIcon(toolName);
}

function toolPathArg(args: any): string | undefined {
  const value = args?.path ?? args?.file_path;
  return typeof value === "string" && value.length > 0 ? value : undefined;
}

function shortToolPath(rawPath: string | undefined, cwd: string | undefined): string {
  if (!rawPath) return "path?";
  if (!isAbsolute(rawPath) || !cwd) return rawPath;

  const relativePath = relative(resolve(cwd), resolve(rawPath));
  const isInsideCwd = relativePath === "" ||
    (relativePath !== ".." && !relativePath.startsWith(`..${sep}`) && !isAbsolute(relativePath));
  return isInsideCwd ? (relativePath || ".") : rawPath;
}

function formatToolLineRange(args: any): string {
  if (args?.offset === undefined && args?.limit === undefined) return "";
  const start = Number.isFinite(Number(args?.offset)) ? Math.max(1, Number(args.offset)) : 1;
  if (args?.limit === undefined) return `:${start}`;
  const end = start + Math.max(0, Number(args.limit) || 0) - 1;
  return end >= start ? `:${start}-${end}` : `:${start}`;
}

function countTextLines(text: string | undefined): number | undefined {
  if (text === undefined) return undefined;
  if (text.length === 0) return 0;
  const normalized = text.replace(/\r\n/g, "\n").replace(/\r/g, "\n").replace(/\n$/, "");
  return normalized.length === 0 ? 1 : normalized.split("\n").length;
}

function plural(count: number, singular: string, pluralForm = `${singular}s`): string {
  return `${count} ${count === 1 ? singular : pluralForm}`;
}

function editCount(args: any): number | undefined {
  if (Array.isArray(args?.edits)) return args.edits.length;
  if (typeof args?.edits === "string") {
    try {
      const parsed = JSON.parse(args.edits);
      if (Array.isArray(parsed)) return parsed.length;
    } catch {
      return undefined;
    }
  }
  if (typeof args?.oldText === "string" && typeof args?.newText === "string") return 1;
  return undefined;
}

function formatToolTitleLine(line: string, summary: string, width: number): string {
  const leadingGutter = line.startsWith(" ") ? " " : "";
  return truncateToWidth(leadingGutter + fg("toolTitle", summary), width, "", true);
}

function compactErrorLines(component: any, width: number): string[] {
  const text = component.result?.content
    ?.filter((content: any) => content?.type === "text")
    ?.map((content: any) => content.text || "")
    ?.join("\n")
    ?.trim();
  if (!text) return [];
  return text.split(/\r?\n/).map((line: string) => truncateToWidth(` ${fg("error", line)}`, width, "", true));
}

function compactToolSummary(component: any): string | undefined {
  if (!toolSummariesEnabled() || component.expanded) return undefined;

  const path = shortToolPath(toolPathArg(component.args), component.cwd);
  switch (component.toolName) {
    case "read":
      return `read ${path}${formatToolLineRange(component.args)}`;
    case "write": {
      if (component.result?.isError) return `write failed ${path}`;
      const content = typeof component.args?.content === "string" ? component.args.content : undefined;
      const lineCount = countTextLines(content);
      const verb = component.result ? "wrote" : "write";
      return lineCount === undefined ? `${verb} ${path}` : `${verb} ${plural(lineCount, "line")} to ${path}`;
    }
    case "edit": {
      const count = editCount(component.args);
      if (component.result?.isError) return `edit failed ${path}`;
      const verb = component.result ? "edited" : "edit";
      return count === undefined ? `${verb} ${path}` : `${verb} ${plural(count, "block")} in ${path}`;
    }
    default:
      return undefined;
  }
}

function applyCompactToolSummary(component: any, lines: string[], titleIndex: number, width: number): string[] {
  const summary = compactToolSummary(component);
  if (!summary) return lines;

  const next = [...lines];
  next[titleIndex] = formatToolTitleLine(next[titleIndex], summary, width);

  if (component.result?.isError && component.toolName !== "bash") {
    const errorLines = compactErrorLines(component, width);
    return errorLines.length > 0 ? [...next.slice(0, titleIndex + 1), ...errorLines] : next.slice(0, titleIndex + 1);
  }

  // Collapsed write previews can be very large. Keep normal write states to one
  // semantic line; edit diffs stay visible because they are the useful payload.
  if (component.toolName === "write") {
    return next.slice(0, titleIndex + 1);
  }
  return next;
}

function firstForegroundAnsi(text: string): string | undefined {
  return text.match(/\x1b\[(?:(?:38;2;\d+;\d+;\d+)|(?:38;5;\d+)|(?:3[0-7])|(?:9[0-7]))m/)?.[0];
}

function colorLikeFollowingText(icon: string, followingText: string): string {
  const ansi = firstForegroundAnsi(followingText);
  return ansi ? `${ansi}${icon}\x1b[39m` : icon;
}

function putIconInLeadingGutter(line: string, icon: string, width: number): string {
  const { prefix, rest } = splitLeadingAnsi(line);
  const restWithoutOneGutterCell = rest.startsWith(" ") ? rest.slice(1) : rest;
  const replacement = `${colorLikeFollowingText(icon, restWithoutOneGutterCell)} `;
  return truncateToWidth(prefix + replacement + restWithoutOneGutterCell, width, "", true);
}

function alignToolContinuationLine(line: string, width: number): string {
  const { prefix, rest } = splitLeadingAnsi(line);
  const restWithoutOneGutterCell = rest.startsWith(" ") ? rest.slice(1) : rest;
  return truncateToWidth(prefix + "  " + restWithoutOneGutterCell, width, "", true);
}

function indentToolOutputLine(line: string, width: number, first: boolean): string {
  const { prefix, rest } = splitLeadingAnsi(line);
  const content = rest.startsWith("  ") ? rest.slice(2) : rest.startsWith(" ") ? rest.slice(1) : rest;
  const branch = first ? "  └  " : "     ";
  return truncateToWidth(prefix + branch + content, width, "", true);
}

function applyToolOutputSubtree(lines: string[], titleIndex: number, width: number, requireSeparator = false, maxOutputLines?: number): string[] {
  let outputStart = titleIndex + 1;
  if (outputStart >= lines.length) return lines;

  if (requireSeparator) {
    const separatorIndex = lines.findIndex((line, index) => index > titleIndex && stripAnsiForTest(line).trim().length === 0);
    if (separatorIndex === -1) return lines;
    outputStart = separatorIndex;
  }

  let firstOutputIndex = outputStart;
  if (stripAnsiForTest(lines[firstOutputIndex]).trim().length === 0) {
    // Pi inserts a separator row between the tool title and output. The subtree
    // marker replaces that row, so output connects directly to the title.
    firstOutputIndex++;
  }
  if (firstOutputIndex >= lines.length) return lines;

  const outputLines = lines.slice(firstOutputIndex);
  const limitedOutputLines = maxOutputLines === undefined ? outputLines : outputLines.slice(0, maxOutputLines);

  return [
    ...lines.slice(0, outputStart),
    ...limitedOutputLines.map((line, index) => indentToolOutputLine(line, width, index === 0)),
  ];
}

function splitToolGutterAndAnsi(line: string): { prefix: string; rest: string } {
  let index = 0;
  if (line.startsWith(" ")) index = 1;
  while (index < line.length) {
    const csi = line.slice(index).match(/^\x1b\[[0-?]*[ -/]*[@-~]/);
    if (csi) {
      index += csi[0].length;
      continue;
    }
    const osc = line.slice(index).match(/^\x1b\][^\x07]*(?:\x07|\x1b\\)/);
    if (osc) {
      index += osc[0].length;
      continue;
    }
    break;
  }
  return { prefix: line.slice(0, index), rest: line.slice(index) };
}

function stripBoldAnsi(line: string): string {
  return line.replace(/\x1b\[1m/g, "").replace(/\x1b\[22m/g, "");
}

function removeBashDollarPrefix(line: string, width: number): string {
  const { prefix, rest } = splitToolGutterAndAnsi(line);
  if (rest.startsWith("$ ")) return truncateToWidth(prefix + rest.slice(2), width, "", true);
  return line;
}

function bashCollapsedCommandLines(line: string, command: string, width: number): string[] {
  const { prefix } = splitToolGutterAndAnsi(line);
  const commandLines = command.split(/\r?\n/);
  const previewCount = Math.min(commandLines.length, getBashCommandPreviewLines());
  const hiddenCount = Math.max(0, commandLines.length - previewCount);

  return commandLines.slice(0, previewCount).map((commandLine, index) => {
    const suffix = index === previewCount - 1 && hiddenCount > 0 ? ` … +${hiddenCount} lines` : "";
    return stripBoldAnsi(truncateToWidth(prefix + commandLine + suffix, width, "…", true));
  });
}

function collapseBashCommandLines(component: any, lines: string[], width: number): string[] {
  if (component.toolName !== "bash") return lines;
  const command = typeof component.args?.command === "string" ? component.args.command : undefined;
  if (!command) return lines.length > 0 ? [stripBoldAnsi(removeBashDollarPrefix(lines[0], width)), ...lines.slice(1)] : lines;

  if (component.expanded) {
    if (lines.length === 0) return lines;
    const resultStartIndex = component.result
      ? lines.findIndex((line, index) => index > 0 && stripAnsiForTest(line).trim().length === 0)
      : -1;
    const commandEndIndex = resultStartIndex === -1 ? lines.length : resultStartIndex;
    const commandLines = lines.slice(0, commandEndIndex).map((line, index) => stripBoldAnsi(index === 0 ? removeBashDollarPrefix(line, width) : line));
    return [...commandLines, ...lines.slice(commandEndIndex)];
  }

  if (lines.length === 0) return lines;
  const resultStartIndex = component.result
    ? lines.findIndex((line, index) => index > 0 && stripAnsiForTest(line).trim().length === 0)
    : -1;
  const commandEndIndex = resultStartIndex === -1 ? lines.length : resultStartIndex;
  const commandLines = bashCollapsedCommandLines(lines[0], command, width);
  return [...commandLines, ...lines.slice(commandEndIndex)];
}

function trimToolPaddingLines(lines: string[]): string[] {
  let start = 0;
  let end = lines.length;
  while (start < end && stripAnsiForTest(lines[start]).trim().length === 0) start++;
  while (end > start && stripAnsiForTest(lines[end - 1]).trim().length === 0) end--;
  return lines.slice(start, end);
}

function stripBackgroundAnsi(line: string): string {
  // Replace Pi's louder tool backgrounds with our own subtle neutral fill,
  // preserving foreground colors, hyperlinks, bold text, etc.
  return line
    .replace(/\x1b\[48;2;\d+;\d+;\d+m/g, "")
    .replace(/\x1b\[48;5;\d+m/g, "")
    .replace(/\x1b\[49m/g, "");
}

function patchToolExecutionRender() {
  const proto = ToolExecutionComponent.prototype as unknown as Record<PropertyKey, unknown>;
  const existing = proto[TOOL_CALL_PATCH_FLAG] as RenderPatchState | undefined;
  if (existing?.version === TOOL_CALL_PATCH_VERSION && typeof existing.originalRender === "function") return;

  restorePrototypeMethods(proto, TOOL_CALL_PATCH_FLAG, [["render", "originalRender"], ["updateDisplay", "originalUpdateDisplay"]]);
  const originalRender = proto.render;
  if (typeof originalRender !== "function") return;

  const originalUpdateDisplay = proto.updateDisplay;
  if (typeof originalUpdateDisplay === "function") {
    proto.updateDisplay = function updateDisplayAndClearToolRenderCache(this: any, ...args: unknown[]) {
      this.__radekToolRenderCache = undefined;
      invalidateChatEntryBlockCache(this);
      return originalUpdateDisplay.apply(this, args);
    };
  }

  proto.render = function renderToolCallWithCaret(this: any, width: number): string[] {
    const originalFlag = this.__radekToolCallCaret;
    if (originalFlag) {
      const cacheKey = toolCacheKey(this.toolName);
      const cache = this.__radekToolRenderCache as LinesCache | undefined;
      if (cache?.version === TOOL_CALL_PATCH_VERSION && cache.width === width && cache.themeKey === cacheKey) {
        return cache.lines;
      }
    }

    this.__radekToolCallCaret = false;
    let lines: string[];
    try {
      lines = (originalRender as Function).call(this, width) as string[];
    } finally {
      this.__radekToolCallCaret = originalFlag;
    }
    if (!originalFlag) return lines;

    const next = collapseBashCommandLines(this, trimToolPaddingLines(lines.map(stripBackgroundAnsi)), width);
    const icon = toolIcon(this.toolName);
    if (!icon) {
      this.__radekToolRenderCache = { version: TOOL_CALL_PATCH_VERSION, width, themeKey: toolCacheKey(this.toolName), lines: next };
      return next;
    }

    const targetIndex = next.findIndex((line) => stripAnsiForTest(line).trim().length > 0);
    if (targetIndex === -1) {
      this.__radekToolRenderCache = { version: TOOL_CALL_PATCH_VERSION, width, themeKey: toolCacheKey(this.toolName), lines: next };
      return next;
    }

    const summarized = applyCompactToolSummary(this, next, targetIndex, width);
    const aligned = summarized.map((line, index) => {
      if (index === targetIndex) return putIconInLeadingGutter(line, icon, width);
      // Keep all following tool block lines aligned under the text that follows
      // the leading icon (same visual pattern as multiline user messages).
      return alignToolContinuationLine(line, width);
    });

    const rendered = this.toolName === "bash" && !this.result
      ? aligned
      : applyToolOutputSubtree(
        aligned,
        targetIndex,
        width,
        this.toolName === "bash",
        this.toolName === "bash" && !this.expanded ? getBashOutputPreviewLines() : undefined,
      );
    this.__radekToolRenderCache = { version: TOOL_CALL_PATCH_VERSION, width, themeKey: toolCacheKey(this.toolName), lines: rendered };
    return rendered;
  };

  proto[TOOL_CALL_PATCH_FLAG] = {
    version: TOOL_CALL_PATCH_VERSION,
    originalRender,
    originalUpdateDisplay,
  };
}

function patchAssistantMessageRender() {
  const proto = AssistantMessageComponent.prototype as unknown as Record<PropertyKey, unknown>;
  const existing = proto[ASSISTANT_MESSAGE_PATCH_FLAG] as RenderPatchState | undefined;
  if (existing?.version === ASSISTANT_MESSAGE_PATCH_VERSION && typeof existing.originalRender === "function") return;

  restorePrototypeMethods(proto, ASSISTANT_MESSAGE_PATCH_FLAG, [["render", "originalRender"], ["updateContent", "originalUpdateContent"]]);
  const originalRender = proto.render;
  if (typeof originalRender !== "function") return;

  const originalUpdateContent = proto.updateContent;
  if (typeof originalUpdateContent === "function") {
    proto.updateContent = function updateContentAndClearAssistantRenderCache(this: any, ...args: unknown[]) {
      this.__radekAssistantRenderCache = undefined;
      invalidateChatEntryBlockCache(this);
      return originalUpdateContent.apply(this, args);
    };
  }

  proto.render = function renderAssistantMessageWithCaret(this: any, width: number): string[] {
    if (!this.__radekAssistantResponseCaret) {
      return (originalRender as Function).call(this, width);
    }

    const cacheKey = themeCacheKey();
    const cache = this.__radekAssistantRenderCache as LinesCache | undefined;
    if (cache?.version === ASSISTANT_MESSAGE_PATCH_VERSION && cache.width === width && cache.themeKey === cacheKey) {
      return cache.lines;
    }

    const caretWidth = visibleWidth(`${assistantFinalIcon()} `);
    if (width <= caretWidth + 1 || !this.contentContainer?.children) {
      const originalFlag = this.__radekAssistantResponseCaret;
      this.__radekAssistantResponseCaret = false;
      try {
        return (originalRender as Function).call(this, width);
      } finally {
        this.__radekAssistantResponseCaret = originalFlag;
      }
    }

    const markerChildIndex = getFirstAssistantTextChildIndex(this);

    const lines: string[] = [];
    const children = this.contentContainer.children as ComponentLike[];
    children.forEach((child, index) => {
      if (index !== markerChildIndex) {
        // Thinking/status blocks inside assistant messages should line up with
        // the text after the assistant marker (column 2), while the marker
        // itself occupies the existing left gutter for final response text.
        lines.push(...addLeftPadding(child.render(Math.max(1, width - 1)), { left: 1, right: 0, contentWidth: Math.max(1, width - 1) }));
        return;
      }

      const childWidth = Math.max(1, width - caretWidth);
      const childLines = child.render(childWidth);
      childLines.forEach((line, lineIndex) => {
        const content = removeMarkdownLeftMargin(line, childWidth);
        lines.push((lineIndex === 0 ? assistantCaret() : " ".repeat(caretWidth)) + content);
      });
    });

    if (!this.hasToolCalls && lines.length > 0) {
      lines[0] = "\x1b]133;A\x07" + lines[0];
      lines[lines.length - 1] = "\x1b]133;B\x07\x1b]133;C\x07" + lines[lines.length - 1];
    }
    this.__radekAssistantRenderCache = { version: ASSISTANT_MESSAGE_PATCH_VERSION, width, themeKey: cacheKey, lines };
    return lines;
  };

  proto[ASSISTANT_MESSAGE_PATCH_FLAG] = {
    version: ASSISTANT_MESSAGE_PATCH_VERSION,
    originalRender,
    originalUpdateContent,
  };
}

function formatTokens(count: number): string {
  if (count < 1000) return count.toString();
  if (count < 10000) return `${(count / 1000).toFixed(1)}k`;
  if (count < 1000000) return `${Math.round(count / 1000)}k`;
  if (count < 10000000) return `${(count / 1000000).toFixed(1)}M`;
  return `${Math.round(count / 1000000)}M`;
}

function sanitizeStatusText(text: string): string {
  return text.replace(/[\r\n\t]/g, " ").replace(/ +/g, " ").trim();
}

function formatCwdForFooter(cwd: string, home: string | undefined): string {
  if (!home) return cwd;
  const resolvedCwd = resolve(cwd);
  const resolvedHome = resolve(home);
  const relativeToHome = relative(resolvedHome, resolvedCwd);
  const isInsideHome = relativeToHome === "" ||
    (relativeToHome !== ".." && !relativeToHome.startsWith(`..${sep}`) && !isAbsolute(relativeToHome));
  if (!isInsideHome) return cwd;
  return relativeToHome === "" ? "~" : `~${sep}${relativeToHome}`;
}

function colorReasoningEffort(level: string, label: string): string {
  const theme = activeTheme();
  if (!theme?.fg) return label;
  switch (level) {
    case "off":
    case "none":
      return theme.fg("thinkingOff", label);
    case "minimal":
      return theme.fg("thinkingMinimal", label);
    case "low":
      return theme.fg("thinkingLow", label);
    case "medium":
      return theme.fg("thinkingMedium", label);
    case "high":
      return theme.fg("thinkingHigh", label);
    case "xhigh":
      return theme.fg("thinkingXhigh", label);
    default:
      return theme.fg("thinkingXhigh", label);
  }
}

function reasoningEffortLabel(level: string): string {
  let label: string;
  switch (level) {
    case "off":
    case "none":
      label = configuredReasoningOffLabel();
      break;
    default: {
      const icon = configuredReasoningIcon(level);
      label = icon ? `${icon} ${level}` : level;
      break;
    }
  }
  return colorReasoningEffort(level, label);
}

function modelStatus(session: any, footerData: any, width: number): string {
  const state = session.state;
  let modelPart = state.model?.id || "no-model";
  if (footerData.getAvailableProviderCount?.() > 1 && state.model) {
    const withProvider = `${modelPart} (${state.model.provider})`;
    if (visibleWidth(withProvider) <= Math.max(12, Math.floor(width * 0.45))) {
      modelPart = withProvider;
    }
  }

  if (!state.model?.reasoning) return dim(modelPart);

  const thinkingLevel = state.thinkingLevel || "off";
  return `${reasoningEffortLabel(thinkingLevel)}${dim(` • ${modelPart}`)}`;
}

function workingDirStatus(session: any, footerData: any): string {
  let pwd = formatCwdForFooter(session.sessionManager.getCwd(), process.env.HOME || process.env.USERPROFILE);
  const branch = footerData.getGitBranch?.();
  if (branch) pwd = `${pwd} (${branch})`;
  const sessionName = session.sessionManager.getSessionName?.();
  if (sessionName) pwd = `${pwd} • ${sessionName}`;
  return pwd;
}

function usageStatus(session: any, autoCompactEnabled: boolean, cacheHolder?: any): string {
  const state = session.state;
  const entries = session.sessionManager.getEntries();
  const contextUsage = session.getContextUsage?.();
  const contextWindow = contextUsage?.contextWindow ?? state.model?.contextWindow ?? 0;
  const contextPercentValue = contextUsage?.percent ?? 0;
  const contextPercentRaw = contextUsage?.percent ?? null;
  const lastEntry = entries[entries.length - 1];
  let latestUsage: any;
  for (let i = entries.length - 1; i >= 0; i--) {
    const entry = entries[i];
    if (entry.type === "message" && entry.message.role === "assistant") {
      latestUsage = entry.message?.usage;
      break;
    }
  }
  const cacheKey = themeCacheKey();
  const cache = cacheHolder?.__radekUsageStatusCache;
  if (cache &&
    cache.version === FOOTER_PATCH_VERSION &&
    cache.themeKey === cacheKey &&
    cache.entryCount === entries.length &&
    cache.lastEntry === lastEntry &&
    cache.latestUsage === latestUsage &&
    cache.contextWindow === contextWindow &&
    cache.contextPercentRaw === contextPercentRaw &&
    cache.autoCompactEnabled === autoCompactEnabled &&
    cache.model === state.model) {
    return cache.text;
  }

  let totalInput = 0;
  let totalOutput = 0;
  let totalCacheRead = 0;
  let totalCacheWrite = 0;
  let totalCost = 0;
  let latestCacheHitRate: number | undefined;

  for (const entry of entries) {
    if (entry.type === "message" && entry.message.role === "assistant") {
      const usage = entry.message.usage ?? {};
      totalInput += usage.input ?? 0;
      totalOutput += usage.output ?? 0;
      totalCacheRead += usage.cacheRead ?? 0;
      totalCacheWrite += usage.cacheWrite ?? 0;
      totalCost += usage.cost?.total ?? 0;
      const latestPromptTokens = (usage.input ?? 0) + (usage.cacheRead ?? 0) + (usage.cacheWrite ?? 0);
      latestCacheHitRate = latestPromptTokens > 0 ? ((usage.cacheRead ?? 0) / latestPromptTokens) * 100 : undefined;
    }
  }

  const contextPercent = contextUsage?.percent !== null ? contextPercentValue.toFixed(1) : "?";
  const autoIndicator = autoCompactEnabled ? " (auto)" : "";
  const contextPercentDisplay = contextPercent === "?"
    ? `?/${formatTokens(contextWindow)}${autoIndicator}`
    : `${contextPercent}%/${formatTokens(contextWindow)}${autoIndicator}`;
  const contextPart = contextPercentValue > 90
    ? fg("error", contextPercentDisplay)
    : contextPercentValue > 70
      ? fg("warning", contextPercentDisplay)
      : dim(contextPercentDisplay);

  const parts: string[] = [];
  if (totalInput) parts.push(dim(`↑${formatTokens(totalInput)}`));
  if (totalOutput) parts.push(dim(`↓${formatTokens(totalOutput)}`));
  if (totalCacheRead) parts.push(dim(`R${formatTokens(totalCacheRead)}`));
  if (totalCacheWrite) parts.push(dim(`W${formatTokens(totalCacheWrite)}`));
  if ((totalCacheRead > 0 || totalCacheWrite > 0) && latestCacheHitRate !== undefined) {
    parts.push(dim(`CH${latestCacheHitRate.toFixed(1)}%`));
  }
  const usingSubscription = state.model
    ? (session.modelRuntime?.isUsingOAuth?.(state.model.provider) ?? session.modelRegistry?.isUsingOAuth?.(state.model) ?? false)
    : false;
  if (totalCost || usingSubscription) {
    parts.push(dim(`$${totalCost.toFixed(3)}${usingSubscription ? " (sub)" : ""}`));
  }
  parts.push(contextPart);
  const text = parts.join(dim(" "));
  if (cacheHolder) {
    cacheHolder.__radekUsageStatusCache = {
      version: FOOTER_PATCH_VERSION,
      themeKey: cacheKey,
      entryCount: entries.length,
      lastEntry,
      latestUsage,
      contextWindow,
      contextPercentRaw,
      autoCompactEnabled,
      model: state.model,
      text,
    };
  }
  return text;
}

function bumpFooterStatusVersion(owner: any) {
  owner.__radekFooterStatusVersion = (owner.__radekFooterStatusVersion ?? 0) + 1;
}

function ensureFooterStatusVersion(footerData: any, statuses: ReadonlyMap<string, string> | undefined): number | undefined {
  if (!footerData || !statuses || typeof statuses !== "object") return undefined;

  const map = statuses as any;
  const existing = map[FOOTER_DATA_PATCH_FLAG] as FooterStatusMapPatchState | undefined;
  if (existing?.version === FOOTER_PATCH_VERSION && existing.owner === footerData) {
    return footerData.__radekFooterStatusVersion ?? 0;
  }

  if (existing?.originalSet) map.set = existing.originalSet;
  if (existing?.originalDelete) map.delete = existing.originalDelete;
  if (existing?.originalClear) map.clear = existing.originalClear;

  const originalSet = typeof map.set === "function" ? map.set : undefined;
  const originalDelete = typeof map.delete === "function" ? map.delete : undefined;
  const originalClear = typeof map.clear === "function" ? map.clear : undefined;

  if (originalSet) {
    map.set = function setAndBumpFooterStatusVersion(this: any, ...args: any[]) {
      const result = originalSet.apply(this, args);
      bumpFooterStatusVersion(footerData);
      return result;
    };
  }
  if (originalDelete) {
    map.delete = function deleteAndBumpFooterStatusVersion(this: any, ...args: any[]) {
      const result = originalDelete.apply(this, args);
      bumpFooterStatusVersion(footerData);
      return result;
    };
  }
  if (originalClear) {
    map.clear = function clearAndBumpFooterStatusVersion(this: any, ...args: any[]) {
      const result = originalClear.apply(this, args);
      bumpFooterStatusVersion(footerData);
      return result;
    };
  }

  if (originalSet || originalDelete || originalClear) {
    map[FOOTER_DATA_PATCH_FLAG] = {
      version: FOOTER_PATCH_VERSION,
      owner: footerData,
      originalSet,
      originalDelete,
      originalClear,
    };
  }

  return footerData.__radekFooterStatusVersion ?? 0;
}

function cachedExtensionStatusText(cacheHolder: any, footerData: any): string {
  const extensionStatuses = footerData.getExtensionStatuses?.() as ReadonlyMap<string, string> | undefined;
  const statusVersion = ensureFooterStatusVersion(footerData, extensionStatuses);
  const cache = cacheHolder.__radekExtensionStatusCache;
  if (cache &&
    cache.version === FOOTER_PATCH_VERSION &&
    cache.statuses === extensionStatuses &&
    cache.statusVersion === statusVersion) {
    return cache.text;
  }

  const text = extensionStatuses && extensionStatuses.size > 0
    ? Array.from(extensionStatuses.entries())
      .sort(([a], [b]) => a.localeCompare(b))
      .map(([, status]) => sanitizeStatusText(status))
      .filter(Boolean)
      .join(" • ")
    : "";

  cacheHolder.__radekExtensionStatusCache = {
    version: FOOTER_PATCH_VERSION,
    statuses: extensionStatuses,
    statusVersion,
    text,
  };
  return text;
}

function patchFooterRender() {
  const proto = FooterComponent.prototype as unknown as Record<PropertyKey, unknown>;
  const existing = proto[FOOTER_PATCH_FLAG] as RenderPatchState | undefined;
  if (existing?.version === FOOTER_PATCH_VERSION && typeof existing.originalRender === "function") return;

  restorePrototypeMethods(proto, FOOTER_PATCH_FLAG, [["render", "originalRender"]]);
  const originalRender = proto.render;
  if (typeof originalRender !== "function") return;

  proto.render = function renderOneLineFooter(this: any, width: number): string[] {
    if (!this.session || !this.footerData) {
      return (originalRender as Function).call(this, width);
    }

    const statusText = cachedExtensionStatusText(this, this.footerData);

    const leftStyled = [
      modelStatus(this.session, this.footerData, width),
      statusText ? dim(statusText) : "",
    ].filter(Boolean).join(dim(" • "));

    const usage = usageStatus(this.session, this.autoCompactEnabled, this);
    const cwd = dim(workingDirStatus(this.session, this.footerData));

    // First line: model/status on the left, current workdir/session on the right.
    const topRight = visibleWidth(cwd) > width - 2
      ? truncateToWidth(cwd, Math.max(1, width - 2), dim("..."))
      : cwd;
    const gap = topRight ? 2 : 0;
    const leftMax = Math.max(1, width - visibleWidth(topRight) - gap);
    const left = truncateToWidth(leftStyled, leftMax, dim("..."));
    const spaces = " ".repeat(Math.max(0, width - visibleWidth(left) - visibleWidth(topRight)));

    // Second line: usage metadata right-aligned underneath.
    const bottom = truncateToWidth(usage, width, dim("..."));
    const bottomSpaces = " ".repeat(Math.max(0, width - visibleWidth(bottom)));
    return [left + spaces + topRight, bottomSpaces + bottom];
  };

  proto[FOOTER_PATCH_FLAG] = { version: FOOTER_PATCH_VERSION, originalRender };
}

function isBlankRenderedLine(line: string): boolean {
  return stripAnsiForTest(line).trim().length === 0;
}

function trimOuterBlankLines(lines: string[]): string[] {
  const cached = trimCache.get(lines);
  if (cached) return cached;

  let start = 0;
  let end = lines.length;
  while (start < end && isBlankRenderedLine(lines[start])) start++;
  while (end > start && isBlankRenderedLine(lines[end - 1])) end--;

  const trimmed = start === 0 && end === lines.length ? lines : lines.slice(start, end);
  trimCache.set(lines, trimmed);
  return trimmed;
}

function invalidateChatEntryBlockCache(entry: ComponentLike | undefined) {
  if (!entry) return;
  entry.__radekChatBlockVersion = (entry.__radekChatBlockVersion ?? 0) + 1;
  entry.__radekChatBlockCache = undefined;
  const owner = entry.__radekChatContainerOwner;
  if (owner) {
    owner.__radekChatContainerVersion = (owner.__radekChatContainerVersion ?? 0) + 1;
    owner.__radekChatContainerCache = undefined;
  }
}

function isCacheableChatEntry(entry: ComponentLike): boolean {
  return entry instanceof UserMessageComponent || entry instanceof AssistantMessageComponent || entry instanceof ToolExecutionComponent;
}

function chatEntryBlockThemeKey(entry: ComponentLike, baseThemeKey: string): string {
  return entry instanceof ToolExecutionComponent ? toolCacheKey((entry as any).toolName, baseThemeKey) : baseThemeKey;
}

function getChatEntryId(entry: ComponentLike): number {
  entry.__radekChatEntryId ??= nextChatEntryId++;
  return entry.__radekChatEntryId;
}

function renderChatEntry(entry: ComponentLike, terminalWidth: number, contentPadding: number): string[] {
  if (entry instanceof UserMessageComponent) {
    entry.__radekFullWidthUserMessage = true;
    try {
      return entry.render(terminalWidth);
    } finally {
      entry.__radekFullWidthUserMessage = false;
    }
  }

  if (entry instanceof AssistantMessageComponent) {
    entry.__radekAssistantResponseCaret = true;
    try {
      // Assistant messages already have their own markdown left padding. The
      // response marker should occupy that existing gutter, not push content
      // one column to the right via the outer conversation padding.
      return entry.render(terminalWidth);
    } finally {
      entry.__radekAssistantResponseCaret = false;
    }
  }

  if (entry instanceof ToolExecutionComponent) {
    entry.__radekToolCallCaret = true;
    try {
      // Tool boxes already contain an internal gutter. Put the tool marker in
      // that gutter instead of adding another outer padding column.
      return entry.render(terminalWidth);
    } finally {
      entry.__radekToolCallCaret = false;
    }
  }

  return renderChildWithPadding(entry, terminalWidth, contentPadding);
}

function renderCachedChatEntryBlock(entry: ComponentLike, terminalWidth: number, contentPadding: number, baseThemeKey: string): string[] {
  if (!isCacheableChatEntry(entry)) {
    return trimOuterBlankLines(renderChatEntry(entry, terminalWidth, contentPadding));
  }

  const themeKey = chatEntryBlockThemeKey(entry, baseThemeKey);
  const mutationVersion = entry.__radekChatBlockVersion ?? 0;
  const cache = entry.__radekChatBlockCache;
  if (cache?.version === CHAT_ENTRY_BLOCK_CACHE_VERSION &&
    cache.width === terminalWidth &&
    cache.contentPadding === contentPadding &&
    cache.themeKey === themeKey &&
    cache.mutationVersion === mutationVersion) {
    return cache.lines;
  }

  const lines = trimOuterBlankLines(renderChatEntry(entry, terminalWidth, contentPadding));
  entry.__radekChatBlockCache = {
    version: CHAT_ENTRY_BLOCK_CACHE_VERSION,
    width: terminalWidth,
    contentPadding,
    themeKey,
    mutationVersion,
    lines,
  };
  return lines;
}

function renderChatContainer(child: ComponentLike, terminalWidth: number, contentPadding: number): ChatContainerRenderResult {
  if (!Array.isArray(child.children)) {
    return { lines: renderChildWithPadding(child, terminalWidth, contentPadding) };
  }

  const baseThemeKey = themeCacheKey();
  const containerVersion = child.__radekChatContainerVersion ?? 0;
  const childCount = child.children.length;
  const lastChild = child.children[childCount - 1];
  const cache = child.__radekChatContainerCache;
  if (cache?.version === CHAT_CONTAINER_CACHE_VERSION &&
    cache.width === terminalWidth &&
    cache.contentPadding === contentPadding &&
    cache.baseThemeKey === baseThemeKey &&
    cache.containerVersion === containerVersion &&
    cache.childCount === childCount &&
    cache.lastChild === lastChild) {
    return { lines: cache.lines, signature: cache.signature, blocks: cache.blocks };
  }

  const lines: string[] = [];
  const signatureParts: string[] = [];
  const blocks: ChatBlockMeta[] = [];
  let signatureAvailable = true;
  for (const entry of child.children) {
    const cacheable = isCacheableChatEntry(entry);
    if (cacheable) {
      // Streaming assistant/tool rows can render empty before their first update.
      // Link them to the container before that empty render so the update can
      // invalidate the outer cache instead of leaving the row invisible forever.
      entry.__radekChatContainerOwner = child;
    }

    const block = renderCachedChatEntryBlock(entry, terminalWidth, contentPadding, baseThemeKey);
    if (block.length === 0) continue;

    const blockStart = lines.length;
    // Normalize top-level conversation spacing: exactly one blank row between
    // every rendered block, regardless of whether Pi inserted Spacer children
    // or the component itself had outer padding.
    if (lines.length > 0) lines.push("");
    lines.push(...block);
    const renderedBlockLength = lines.length - blockStart;

    if (!cacheable) {
      signatureAvailable = false;
      continue;
    }

    const blockKey = `${getChatEntryId(entry)}:${entry.__radekChatBlockVersion ?? 0}:${renderedBlockLength}:${chatEntryBlockThemeKey(entry, baseThemeKey)}`;
    if (signatureAvailable) {
      signatureParts.push(blockKey);
      blocks.push({ key: blockKey, start: blockStart, length: renderedBlockLength });
    }
  }

  const signature = signatureAvailable ? `${terminalWidth}:${contentPadding}:${lines.length}:${signatureParts.join("|")}` : undefined;
  if (signature) {
    child.__radekChatContainerCache = {
      version: CHAT_CONTAINER_CACHE_VERSION,
      width: terminalWidth,
      contentPadding,
      baseThemeKey,
      containerVersion,
      childCount,
      lastChild,
      lines,
      signature,
      blocks,
    };
  }
  return { lines, signature, blocks: signature ? blocks : undefined };
}

function isTerminalImageLine(line: string): boolean {
  return line.includes("\x1b_G") || line.includes("\x1b]1337;File=");
}

function normalizeTerminalOutputCompat(line: string): string {
  if (!THAI_LAO_AM_REGEX.test(line)) return line;
  return line.replace(THAI_LAO_AM_GLOBAL_REGEX, (char) => char === "\u0e33" ? "\u0e4d\u0e32" : "\u0ecd\u0eb2");
}

function resetNormalizedLine(line: string): string {
  return isTerminalImageLine(line) ? line : normalizeTerminalOutputCompat(line) + TERMINAL_SEGMENT_RESET;
}

function findRawCursorPosition(lines: string[], height: number): { row: number; col: number } | null {
  const viewportTop = Math.max(0, lines.length - height);
  for (let row = lines.length - 1; row >= viewportTop; row--) {
    const line = lines[row] ?? "";
    const markerIndex = line.indexOf(CURSOR_MARKER);
    if (markerIndex !== -1) {
      return { row, col: visibleWidth(line.slice(0, markerIndex)) };
    }
  }
  return null;
}

function rawLineWithoutCursorMarker(line: string, row: number, cursorPos: { row: number; col: number } | null): string {
  if (!cursorPos || cursorPos.row !== row) return line;
  const markerIndex = line.indexOf(CURSOR_MARKER);
  return markerIndex === -1 ? line : line.slice(0, markerIndex) + line.slice(markerIndex + CURSOR_MARKER.length);
}

function normalizedRawLineAt(lines: string[], row: number, cursorPos: { row: number; col: number } | null): string {
  return resetNormalizedLine(rawLineWithoutCursorMarker(lines[row] ?? "", row, cursorPos));
}

function rangesMatchPreviousLines(previousLines: string[], rawLines: string[], cursorPos: { row: number; col: number } | null, start: number, end: number): boolean {
  for (let i = start; i < end; i++) {
    if (previousLines[i] !== normalizedRawLineAt(rawLines, i, cursorPos)) return false;
  }
  return true;
}

function tryFastNoopRender(tui: MaybeTui, rawLines: string[], width: number, height: number): boolean {
  const previousLines = tui.previousLines;
  const previousMeta = tui.__radekPreviousFrameMeta;
  const currentMeta = tui.__radekCurrentFrameMeta;
  if (!previousLines?.length || !previousMeta || !currentMeta || !currentMeta.chatSignature) return false;
  if (tui.previousWidth !== width || tui.previousHeight !== height) return false;
  if (previousLines.length !== rawLines.length || currentMeta.totalLines !== rawLines.length) return false;
  if (previousMeta.width !== currentMeta.width || previousMeta.width !== width) return false;
  if (previousMeta.chatStart !== currentMeta.chatStart || previousMeta.chatLineCount !== currentMeta.chatLineCount) return false;
  if (previousMeta.chatSignature !== currentMeta.chatSignature) return false;

  const cursorPos = findRawCursorPosition(rawLines, height);
  const chatStart = currentMeta.chatStart;
  const chatEnd = chatStart + currentMeta.chatLineCount;
  if (!rangesMatchPreviousLines(previousLines, rawLines, cursorPos, 0, chatStart)) return false;
  if (!rangesMatchPreviousLines(previousLines, rawLines, cursorPos, chatEnd, rawLines.length)) return false;

  tui.positionHardwareCursor?.(cursorPos, rawLines.length);
  tui.previousViewportTop = tui.previousViewportTop ?? 0;
  tui.previousWidth = width;
  tui.previousHeight = height;
  tui.__radekPreviousFrameMeta = currentMeta;
  return true;
}

function firstChangedFromChatMeta(previousMeta: ChatFrameMeta, currentMeta: ChatFrameMeta): number | undefined {
  if (previousMeta.width !== currentMeta.width) return undefined;
  if (previousMeta.chatStart !== currentMeta.chatStart) return undefined;
  const previousBlocks = previousMeta.chatBlocks;
  const currentBlocks = currentMeta.chatBlocks;
  if (!previousBlocks || !currentBlocks) return undefined;

  const commonLength = Math.min(previousBlocks.length, currentBlocks.length);
  let firstDiffBlock = 0;
  while (firstDiffBlock < commonLength && previousBlocks[firstDiffBlock].key === currentBlocks[firstDiffBlock].key) {
    firstDiffBlock++;
  }

  if (firstDiffBlock === previousBlocks.length && firstDiffBlock === currentBlocks.length) {
    return currentMeta.chatStart + currentMeta.chatLineCount;
  }

  const previousOffset = firstDiffBlock < previousBlocks.length ? previousBlocks[firstDiffBlock].start : previousMeta.chatLineCount;
  const currentOffset = firstDiffBlock < currentBlocks.length ? currentBlocks[firstDiffBlock].start : currentMeta.chatLineCount;
  if (previousOffset !== currentOffset) return undefined;
  return currentMeta.chatStart + currentOffset;
}

function suffixHasTerminalImages(previousLines: string[], rawLines: string[], start: number): boolean {
  for (let i = start; i < previousLines.length; i++) {
    if (isTerminalImageLine(previousLines[i] ?? "")) return true;
  }
  for (let i = start; i < rawLines.length; i++) {
    if (isTerminalImageLine(rawLines[i] ?? "")) return true;
  }
  return false;
}

function buildNormalizedSuffixLines(rawLines: string[], cursorPos: { row: number; col: number } | null, start: number, width: number): string[] | undefined {
  const lines: string[] = [];
  for (let i = start; i < rawLines.length; i++) {
    const line = normalizedRawLineAt(rawLines, i, cursorPos);
    if (!isTerminalImageLine(line) && visibleWidth(line) > width) return undefined;
    lines.push(line);
  }
  return lines;
}

function tryFastSuffixRender(tui: MaybeTui, rawLines: string[], width: number, height: number): boolean {
  const previousLines = tui.previousLines;
  const previousMeta = tui.__radekPreviousFrameMeta;
  const currentMeta = tui.__radekCurrentFrameMeta;
  const write = tui.terminal?.write;
  if (!previousLines?.length || !previousMeta || !currentMeta || typeof write !== "function") return false;
  if (tui.previousWidth !== width || tui.previousHeight !== height) return false;
  if (previousLines.length > rawLines.length) return false;
  if (currentMeta.totalLines !== rawLines.length) return false;

  const firstChanged = firstChangedFromChatMeta(previousMeta, currentMeta);
  if (firstChanged === undefined || firstChanged >= rawLines.length) return false;

  const cursorPos = findRawCursorPosition(rawLines, height);
  if (!rangesMatchPreviousLines(previousLines, rawLines, cursorPos, 0, currentMeta.chatStart)) return false;
  if (firstChanged < currentMeta.chatStart && !rangesMatchPreviousLines(previousLines, rawLines, cursorPos, currentMeta.chatStart, firstChanged)) return false;

  const prevViewportTop = tui.previousViewportTop ?? 0;
  if (firstChanged < prevViewportTop) return false;
  if (suffixHasTerminalImages(previousLines, rawLines, firstChanged)) return false;
  const suffixLines = buildNormalizedSuffixLines(rawLines, cursorPos, firstChanged, width);
  if (!suffixLines) return false;

  let viewportTop = prevViewportTop;
  let hardwareCursorRow = tui.hardwareCursorRow ?? Math.max(0, previousLines.length - 1);
  const prevViewportBottom = prevViewportTop + height - 1;
  let buffer = "\x1b[?2026h";

  if (firstChanged > prevViewportBottom) {
    const currentScreenRow = Math.max(0, Math.min(height - 1, hardwareCursorRow - prevViewportTop));
    const moveToBottom = height - 1 - currentScreenRow;
    if (moveToBottom > 0) buffer += `\x1b[${moveToBottom}B`;
    const scroll = firstChanged - prevViewportBottom;
    buffer += "\r\n".repeat(scroll);
    viewportTop += scroll;
    hardwareCursorRow = firstChanged;
  }

  const currentScreenRow = hardwareCursorRow - viewportTop;
  const targetScreenRow = firstChanged - viewportTop;
  const lineDiff = targetScreenRow - currentScreenRow;
  if (lineDiff > 0) buffer += `\x1b[${lineDiff}B`;
  else if (lineDiff < 0) buffer += `\x1b[${-lineDiff}A`;
  buffer += "\r";

  for (let i = 0; i < suffixLines.length; i++) {
    if (i > 0) buffer += "\r\n";
    buffer += "\x1b[2K" + suffixLines[i];
  }
  buffer += "\x1b[?2026l";
  write.call(tui.terminal, buffer);

  const nextPreviousLines = previousLines.slice(0, firstChanged);
  nextPreviousLines.push(...suffixLines);
  tui.previousLines = nextPreviousLines;
  tui.previousWidth = width;
  tui.previousHeight = height;
  tui.cursorRow = Math.max(0, rawLines.length - 1);
  tui.hardwareCursorRow = Math.max(0, rawLines.length - 1);
  tui.maxLinesRendered = Math.max(tui.maxLinesRendered ?? 0, rawLines.length);
  tui.previousViewportTop = Math.max(viewportTop, (tui.hardwareCursorRow ?? 0) - height + 1);
  tui.__radekPreviousFrameMeta = currentMeta;
  tui.positionHardwareCursor?.(cursorPos, rawLines.length);
  return true;
}

function clampNumber(value: number, min: number, max: number): number {
  return Math.max(min, Math.min(max, value));
}

function renderStickyPromptFrame(tui: MaybeTui, scrollableLines: string[], stickyLines: string[], height: number): string[] {
  if (height <= 0) return [...scrollableLines, ...stickyLines];

  tui.__radekStickyPromptActive = true;

  const stickyVisible = stickyLines.length > height ? stickyLines.slice(stickyLines.length - height) : stickyLines;
  const viewportHeight = Math.max(0, height - stickyVisible.length);
  const previousTotal = tui.__radekStickyScrollTotalLines;
  let offset = Math.max(0, tui.__radekStickyScrollOffset ?? 0);

  const transcriptGrew = previousTotal !== undefined && scrollableLines.length > previousTotal;
  if (tui.__radekStickyJumpOnTranscriptGrowth && transcriptGrew) {
    offset = 0;
    tui.__radekStickyJumpOnTranscriptGrowth = false;
  } else if (offset > 0 && transcriptGrew) {
    // If the user is scrolled up and new transcript lines arrive, preserve the
    // same visible content instead of pulling the viewport toward the bottom.
    offset += scrollableLines.length - previousTotal;
  }

  const maxOffset = Math.max(0, scrollableLines.length - viewportHeight);
  offset = clampNumber(offset, 0, maxOffset);

  tui.__radekStickyScrollOffset = offset;
  tui.__radekStickyScrollMaxOffset = maxOffset;
  tui.__radekStickyScrollTotalLines = scrollableLines.length;
  tui.__radekStickyScrollViewportHeight = viewportHeight;

  const end = Math.max(0, scrollableLines.length - offset);
  const start = Math.max(0, end - viewportHeight);
  const visibleScrollable = viewportHeight > 0 ? scrollableLines.slice(start, end) : [];
  const topBlankLines = Math.max(0, viewportHeight - visibleScrollable.length);

  return [
    ...Array.from({ length: topBlankLines }, () => ""),
    ...visibleScrollable,
    ...stickyVisible,
  ];
}

function hasVisibleOverlay(tui: MaybeTui): boolean {
  if (!Array.isArray(tui.overlayStack) || tui.overlayStack.length === 0) return false;
  if (typeof tui.isOverlayVisible !== "function") return true;
  return tui.overlayStack.some((entry) => tui.isOverlayVisible?.(entry));
}

function setMouseReporting(tui: MaybeTui, enabled: boolean) {
  const write = tui.terminal?.write;
  if (typeof write !== "function") return;
  if (tui.__radekMouseReportingEnabled === enabled) return;

  write.call(tui.terminal, enabled ? MOUSE_REPORTING_ENABLE : MOUSE_REPORTING_DISABLE);
  tui.__radekMouseReportingEnabled = enabled;
}

function parseMouseInput(data: string): { kind: "wheel"; direction: "up" | "down" | "left" | "right" } | { kind: "other" } | undefined {
  const sgrMatch = data.match(/^\x1b\[<(\d+);(\d+);(\d+)([mM])$/);
  if (sgrMatch) {
    const buttonCode = Number(sgrMatch[1]);
    if (!Number.isFinite(buttonCode)) return { kind: "other" };
    const baseButton = buttonCode & ~28; // Strip Shift/Alt/Ctrl modifier bits.
    if (sgrMatch[4] === "M") {
      if (baseButton === 64) return { kind: "wheel", direction: "up" };
      if (baseButton === 65) return { kind: "wheel", direction: "down" };
      if (baseButton === 66) return { kind: "wheel", direction: "left" };
      if (baseButton === 67) return { kind: "wheel", direction: "right" };
    }
    return { kind: "other" };
  }

  if (data.startsWith("\x1b[M") && data.length >= 6) {
    const buttonCode = data.charCodeAt(3) - 32;
    const baseButton = buttonCode & ~28;
    if (baseButton === 64) return { kind: "wheel", direction: "up" };
    if (baseButton === 65) return { kind: "wheel", direction: "down" };
    if (baseButton === 66) return { kind: "wheel", direction: "left" };
    if (baseButton === 67) return { kind: "wheel", direction: "right" };
    return { kind: "other" };
  }

  return undefined;
}

function editorText(editor: Editor): string {
  const anyEditor = editor as any;
  const getExpandedText = anyEditor.getExpandedText;
  if (typeof getExpandedText === "function") return String(getExpandedText.call(editor) ?? "");
  const getText = anyEditor.getText;
  if (typeof getText === "function") return String(getText.call(editor) ?? "");
  return "";
}

function submitShouldJumpToBottom(tui: MaybeTui, data: string): { editor: Editor; beforeText: string } | undefined {
  if (isKeyRelease(data)) return undefined;
  const editor = tui.focusedComponent;
  if (!(editor instanceof Editor)) return undefined;
  if ((editor as any).disableSubmit) return undefined;

  const keybindings = getKeybindings();
  if (!keybindings.matches(data, "tui.input.submit")) return undefined;

  const beforeText = editorText(editor);
  return beforeText.trim().length > 0 ? { editor, beforeText } : undefined;
}

function markJumpToSubmittedMessage(tui: MaybeTui, submit: { editor: Editor; beforeText: string } | undefined) {
  if (!submit) return;
  const afterText = editorText(submit.editor);
  if (afterText.trim().length > 0 || afterText === submit.beforeText) return;

  tui.__radekStickyScrollOffset = 0;
  tui.__radekStickyJumpOnTranscriptGrowth = true;
}

function focusedEditorHasScrollablePrompt(tui: MaybeTui): boolean {
  const editor = tui.focusedComponent;
  if (!(editor instanceof Editor)) return false;
  const layoutText = (editor as any).layoutText;
  if (typeof layoutText !== "function") return false;

  const layoutWidth = Number((editor as any).lastWidth) || Math.max(1, Number(tui.terminal?.columns) || 80);
  const layoutLines = layoutText.call(editor, layoutWidth);
  if (!Array.isArray(layoutLines)) return false;

  const terminalRows = Number(tui.terminal?.rows) || 24;
  const maxVisibleLines = Math.max(5, Math.floor(terminalRows * 0.3));
  return layoutLines.length > maxVisibleLines;
}

function scrollStepForInput(tui: MaybeTui, data: string): number | "top" | "bottom" | undefined {
  if (isKeyRelease(data)) return undefined;

  const maxOffset = tui.__radekStickyScrollMaxOffset ?? 0;
  const offset = tui.__radekStickyScrollOffset ?? 0;
  const viewportHeight = tui.__radekStickyScrollViewportHeight ?? (Number(tui.terminal?.rows) || 24);
  const pageSize = Math.max(1, viewportHeight - 2);
  const promptCanPage = focusedEditorHasScrollablePrompt(tui);
  const pageUp = matchesKey(data, "pageUp");
  const pageDown = matchesKey(data, "pageDown");
  const shiftPageUp = matchesKey(data, "shift+pageUp");
  const shiftPageDown = matchesKey(data, "shift+pageDown");

  if ((shiftPageUp || (pageUp && !promptCanPage))) return maxOffset > 0 ? pageSize : undefined;
  if ((shiftPageDown || (pageDown && !promptCanPage))) return offset > 0 ? -pageSize : undefined;
  if (matchesKey(data, "alt+up")) return maxOffset > 0 ? 1 : undefined;
  if (matchesKey(data, "alt+down")) return offset > 0 ? -1 : undefined;
  if (matchesKey(data, "ctrl+home")) return maxOffset > 0 ? "top" : undefined;
  if (matchesKey(data, "ctrl+end")) return offset > 0 ? "bottom" : undefined;

  return undefined;
}

function applyStickyScrollStep(tui: MaybeTui, step: number | "top" | "bottom"): boolean {
  const maxOffset = tui.__radekStickyScrollMaxOffset ?? 0;
  const currentOffset = tui.__radekStickyScrollOffset ?? 0;
  const nextOffset = step === "top"
    ? maxOffset
    : step === "bottom"
      ? 0
      : clampNumber(currentOffset + step, 0, maxOffset);

  if (nextOffset === currentOffset) return false;

  tui.__radekStickyScrollOffset = nextOffset;
  tui.requestRender?.();
  return true;
}

function handleStickyScrollInput(tui: MaybeTui, data: string): boolean {
  const mouse = parseMouseInput(data);
  if (mouse) {
    // Mouse reporting turns clicks/wheel gestures into escape sequences. Pi TUI
    // has no native mouse handling, so consume all of them to avoid leaking
    // bytes into the prompt; only wheel vertical gestures scroll the transcript.
    if (mouse.kind !== "wheel" || mouse.direction === "left" || mouse.direction === "right") return true;
    if (!tui.__radekStickyPromptActive || hasVisibleOverlay(tui)) return true;
    if (tui.focusedComponent && !(tui.focusedComponent instanceof Editor)) return true;
    applyStickyScrollStep(tui, mouse.direction === "up" ? MOUSE_WHEEL_SCROLL_LINES : -MOUSE_WHEEL_SCROLL_LINES);
    return true;
  }

  if (!tui.__radekStickyPromptActive || hasVisibleOverlay(tui)) return false;
  if (tui.focusedComponent && !(tui.focusedComponent instanceof Editor)) return false;

  const step = scrollStepForInput(tui, data);
  if (step === undefined) return false;

  return applyStickyScrollStep(tui, step);
}

export default function (pi?: any) {
  restoreLegacyThemeMonkeyPatches();

  pi?.on?.("session_start", (_event: unknown, ctx: any) => {
    restoreLegacyThemeMonkeyPatches(ctx.ui.theme);
    currentTheme = ctx.ui.theme;
  });
  patchMarkdownBaseTextColor();
  patchEditorRender();
  patchUserMessageRender();
  patchToolExecutionRender();
  patchAssistantMessageRender();
  patchFooterRender();
  const proto = TUI.prototype as unknown as Record<PropertyKey, unknown>;
  const existing = proto[PATCH_FLAG] as PatchState | undefined;
  if (existing?.version === PATCH_VERSION && typeof existing.originalDoRender === "function") return;

  // If a newer/older version of this extension is already active in this Pi
  // process, restore the native renderer before applying the padding patch.
  // That keeps /reload and source edits from building wrapper chains.
  restorePrototypeMethods(proto, PATCH_FLAG, [
    ["doRender", "originalDoRender"],
    ["handleInput", "originalHandleInput"],
    ["start", "originalStart"],
    ["stop", "originalStop"],
  ]);

  const originalDoRender = proto.doRender;
  const originalHandleInput = proto.handleInput;
  const originalStart = proto.start;
  const originalStop = proto.stop;
  if (typeof originalDoRender !== "function") return;

  if (typeof originalHandleInput === "function") {
    proto.handleInput = function patchedHandleInput(this: MaybeTui, data: string, ...inputArgs: unknown[]) {
      if (typeof data === "string" && handleStickyScrollInput(this, data)) return;
      const submit = typeof data === "string" ? submitShouldJumpToBottom(this, data) : undefined;
      const result = (originalHandleInput as Function).apply(this, [data, ...inputArgs]);
      markJumpToSubmittedMessage(this, submit);
      return result;
    };
  }

  if (typeof originalStart === "function") {
    proto.start = function patchedStart(this: MaybeTui, ...startArgs: unknown[]) {
      const result = (originalStart as Function).apply(this, startArgs);
      setMouseReporting(this, true);
      return result;
    };
  }

  if (typeof originalStop === "function") {
    proto.stop = function patchedStop(this: MaybeTui, ...stopArgs: unknown[]) {
      setMouseReporting(this, false);
      return (originalStop as Function).apply(this, stopArgs);
    };
  }

  proto.doRender = function patchedDoRender(this: MaybeTui, ...args: unknown[]) {
    // Use the terminal's real hardware cursor for prompt blinking. The editor
    // still emits CURSOR_MARKER, so TUI knows where to place it.
    (this as any).showHardwareCursor = true;
    setMouseReporting(this, true);
    const terminalWidth = Number(this.terminal?.columns) || 80;
    const contentPadding = getContentPadding();
    const promptPadding = getPromptPadding();

    const originalRender = this.render;
    const originalCompositeOverlays = this.compositeOverlays;
    const originalGetHorizontalPadding = this.getHorizontalPadding;
    const originalApplyHorizontalPadding = this.applyHorizontalPadding;

    // Disable any in-place dist patch while this persistent extension supplies
    // mixed padding. This prevents double-padding if a local dist patch exists.
    if (typeof originalGetHorizontalPadding === "function") {
      this.getHorizontalPadding = (width: number) => ({ left: 0, right: 0, contentWidth: Math.max(1, width) });
    }
    if (typeof originalApplyHorizontalPadding === "function") {
      this.applyHorizontalPadding = (lines: string[]) => lines;
    }

    this.render = function renderWithMixedPadding(this: MaybeTui, _width: number) {
      this.__radekCurrentFrameMeta = undefined;
      this.__radekStickyPromptActive = false;
      if (!Array.isArray(this.children) || this.children.length === 0) {
        return addLeftPadding(originalRender.call(this, getPadding(terminalWidth, contentPadding).contentWidth), getPadding(terminalWidth, contentPadding));
      }

      const scrollableLines: string[] = [];
      const stickyLines: string[] = [];
      const childCount = this.children.length;
      this.children.forEach((child, index) => {
        if (isChatContainerChild(index, childCount)) {
          const chat = renderChatContainer(child, terminalWidth, contentPadding);
          scrollableLines.push(...chat.lines);
          return;
        }

        if (isEditorContainerChild(index, childCount)) {
          // Editor horizontal rules span the full terminal width. The editor
          // render patch adds the caret and textarea text offset internally.
          stickyLines.push(...renderPromptEditorChild(child, terminalWidth));
          return;
        }

        const requestedPadding = isPromptAreaChild(index, childCount) ? promptPadding : contentPadding;
        const renderedLines = renderChildWithPadding(child, terminalWidth, requestedPadding);
        if (isPromptAreaChild(index, childCount)) {
          stickyLines.push(...renderedLines);
        } else {
          scrollableLines.push(...renderedLines);
        }
      });

      const terminalHeight = Number(this.terminal?.rows) || 24;
      return renderStickyPromptFrame(this, scrollableLines, stickyLines, terminalHeight);
    };

    // Base content is already padded by renderWithMixedPadding. Let overlays keep
    // the full terminal width instead of being clipped to conversation padding.
    if (typeof originalCompositeOverlays === "function") {
      this.compositeOverlays = function compositeOverlaysFullWidth(this: MaybeTui, lines: string[], _termWidth: number, termHeight: number) {
        return originalCompositeOverlays.call(this, lines, terminalWidth, termHeight) as string[];
      };
    }

    try {
      const hasOverlays = Array.isArray(this.overlayStack) && this.overlayStack.length > 0;
      if (!hasOverlays) {
        const preRenderedLines = this.render(terminalWidth);
        const terminalHeight = Number(this.terminal?.rows) || 24;
        if (tryFastNoopRender(this, preRenderedLines, terminalWidth, terminalHeight)) {
          return;
        }
        if (tryFastSuffixRender(this, preRenderedLines, terminalWidth, terminalHeight)) {
          return;
        }
        // Let Pi's native renderer handle the non-trivial diff, but don't make
        // it render the whole tree a second time just because this fast path
        // already produced the frame.
        this.render = () => preRenderedLines;
      }

      const result = (originalDoRender as Function).apply(this, args);
      this.__radekPreviousFrameMeta = this.__radekCurrentFrameMeta;
      return result;
    } finally {
      this.render = originalRender;
      this.compositeOverlays = originalCompositeOverlays;
      this.getHorizontalPadding = originalGetHorizontalPadding;
      this.applyHorizontalPadding = originalApplyHorizontalPadding;
    }
  };

  proto[PATCH_FLAG] = { version: PATCH_VERSION, originalDoRender, originalHandleInput, originalStart, originalStop };
}
