#!/usr/bin/env node
import { performance } from "node:perf_hooks";
import { createJiti } from "/Users/radek/.nvm/versions/node/v24.17.0/lib/node_modules/@earendil-works/pi-coding-agent/node_modules/jiti/lib/jiti.mjs";

const piRoot = "/Users/radek/.nvm/versions/node/v24.17.0/lib/node_modules/@earendil-works/pi-coding-agent";
const piPkg = `${piRoot}/dist/index.js`;
const piTui = `${piRoot}/node_modules/@earendil-works/pi-tui/dist/index.js`;
const extensionPath = new URL("../extensions/rdksl-ui.ts", import.meta.url).pathname;

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
console.log(JSON.stringify({
  mode,
  pairs,
  iterations,
  firstMs: Number(first.toFixed(3)),
  avgNoopMs: Number(avg.toFixed(3)),
  minNoopMs: Number(Math.min(...samples).toFixed(3)),
  maxNoopMs: Number(Math.max(...samples).toFixed(3)),
  lines: tui.previousLines.length,
}, null, 2));
