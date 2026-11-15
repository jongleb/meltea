import { existsSync } from "node:fs";
import { createRequire } from "node:module";
import { join, resolve } from "node:path";
import { defineConfig } from "vitest/config";

// Melange emits library imports as bare paths ("melange.js/caml.mjs") into the
// emit target's node_modules, which has no package.json for Vite to find.
const melangeOutput = {
  name: "melange-output",
  resolveId(source, importer) {
    const output = importer?.match(/^(.*\/_build\/default\/.*?\/output)\//);
    if (!output || source.startsWith(".") || source.startsWith("/")) return null;
    const candidate = join(output[1], "node_modules", source);
    return existsSync(candidate) ? candidate : null;
  },
};

// The React 19 dual tests get React 19 from their own folder; everything else keeps React 18.
const react19 = createRequire(resolve("test/dual19/env/package.json"));
const reactNineteen = {
  name: "react-19-for-dual19",
  enforce: "pre",
  resolveId(source, importer) {
    if (!importer?.includes("/test/dual19/react/output/")) return null;
    if (!/^react(-dom)?(\/|$)/.test(source)) return null;
    return react19.resolve(source);
  },
};

// The mixed compat laws bind JavaScript components from mlx; each build takes them on its own React:
// melact.compat's for the Melact build, npm's for the reason-react build and the build with islands.
const compatEnv = resolve("test/compat/env/libraries.mjs");
const compatLibraries = {
  name: "compat-libraries",
  enforce: "pre",
  async resolveId(source, importer) {
    const melact = importer?.includes("/test/compat/melact/output/");
    const react = importer?.includes("/test/compat/react/output/") || importer?.includes("/test/compat/island/output/");
    if (!melact && !react) return null;
    if (source === "compat-layers") return resolve(melact ? "test/compat/env/layers.mjs" : "test/compat/layers-react.mjs");
    if (melact && /^(react-select|react-window|@radix-ui\/|react-dnd|dnd-core)/.test(source)) return this.resolve(source, compatEnv, { skipSelf: true });
    return null;
  },
};

// Under vitest every package resolves as npm resolves it, so an import of melact.island's colour is the
// package itself; test/compat/bundle.test.mjs checks the colours with esbuild and melact.island's plugin.
const islandColour = {
  name: "melact-island-colour",
  enforce: "pre",
  resolveId(source, importer) {
    if (!source.startsWith("melact-island:")) return null;
    return this.resolve(source.slice("melact-island:".length), importer, { skipSelf: true });
  },
};

export default defineConfig({
  plugins: [islandColour, reactNineteen, compatLibraries, melangeOutput],
  // The emitted libraries live under node_modules; pre-bundling would cache
  // them across dune rebuilds.
  optimizeDeps: { noDiscovery: true, include: [] },
  test: {
    globals: true,
    environment: "jsdom",
    include: ["_build/default/test/**/output/test/**/*_test.mjs", "test/compat/*.test.mjs", "test/team/*.test.mjs"],
    // test/typed/dual and test/typed/staged are the dual suite under staged_pps with and without -typed, checked
    // byte for byte against each other by dune; test/typed/probed runs it.
    exclude: ["**/node_modules/**", "_build/default/test/typed/dual/**", "_build/default/test/typed/staged/**"],
    setupFiles: ["./test/setup.js"],
    // A property law runs its body up to a hundred times; beside the ratchet and the prerendered laws, which
    // take minutes on every core, one can exceed vitest's 5 s without being stuck.
    testTimeout: 30000,
    // The ratchet's leak check collects garbage before it looks for nodes still reachable after an unmount.
    poolOptions: { forks: { execArgv: ["--expose-gc"] } },
    // The size ratchet bundles with esbuild, which needs Node's own typed arrays.
    environmentMatchGlobs: [["**/test/ratchet/sizes/sizes_test.mjs", "node"]],
    // The compat laws load the npm libraries twice: from the root on npm's React, and from
    // test/compat/env on melact.compat, through Vite so that they share Melact's runtime with the app.
    // melact.island imports npm's React as `melact-island:react`, which only Vite resolves (islandColour above).
    server: { deps: { inline: [/test\/compat\/env\/node_modules\/(?!hoist-non-react-statics|react-is|prop-types)/, /\/node_modules\/melact\.island\//] } },
  },
});
