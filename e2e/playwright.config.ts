/**
 * The web editor in Chromium, against the real server: the flake's anima2d (which serves
 * the built editor too), on a port of its own with an empty data folder for the run.
 * ANIMA2D_BIN names the server; `just web-ui` builds it and runs these.
 */
import { defineConfig, devices } from "@playwright/test";

const port = 19481;
const server = process.env.ANIMA2D_BIN ?? "anima2d";

export default defineConfig({
  testDir: ".",
  testMatch: "*.e2e.ts",
  fullyParallel: false,
  workers: 1,
  retries: process.env.CI === undefined ? 0 : 1,
  reporter: [["list"], ["html", { open: "never" }]],
  timeout: 30_000,
  use: {
    baseURL: `http://127.0.0.1:${port}`,
    trace: "retain-on-failure",
    screenshot: "only-on-failure",
    serviceWorkers: "block",
  },
  projects: [{ name: "chromium", use: { ...devices["Desktop Chrome"], viewport: { width: 1360, height: 860 } } }],
  webServer: {
    command: `sh -c 'd=$(mktemp -d); exec "${server}" serve --port ${port} --data "$d"'`,
    url: `http://127.0.0.1:${port}/api/projects`,
    reuseExistingServer: false,
    timeout: 60_000,
  },
});
