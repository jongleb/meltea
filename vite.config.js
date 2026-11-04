import { existsSync } from "node:fs";
import { join } from "node:path";
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

export default defineConfig({
  plugins: [melangeOutput],
  // The emitted libraries live under node_modules; pre-bundling would cache
  // them across dune rebuilds.
  optimizeDeps: { noDiscovery: true, include: [] },
  test: {
    globals: true,
    environment: "jsdom",
    include: ["_build/default/test/**/output/test/**/*_test.mjs"],
    setupFiles: ["./test/setup.js"],
  },
});
