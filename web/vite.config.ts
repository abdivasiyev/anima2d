import { defineConfig } from "vitest/config";

export default defineConfig({
  server: {
    // `cabal run anima2d -- serve --data ../data --port 9215` serves the API in development.
    proxy: { "/api": "http://127.0.0.1:9215" },
  },
  build: { target: "safari16", sourcemap: true },
  test: { environment: "node" },
});
