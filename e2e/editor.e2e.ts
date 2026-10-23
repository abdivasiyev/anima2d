/**
 * The web editor as a person uses it: a project made from the list, a stroke drawn with
 * the mouse and saved to the server, undone, a layer added, the reference board and the
 * export dialog opened, the playhead stepped. What the server holds is read back through
 * its API, so a check is of the saved project, not of what the page shows.
 */
import { expect, test, type APIRequestContext, type Page } from "@playwright/test";

/** The project as the server has it now. */
const stored = async (request: APIRequestContext, id: string): Promise<{ layers: { name: string; cels?: { items: unknown[] }[] }[] }> => {
  const r = await request.get(`/api/projects/${id}`);
  expect(r.ok()).toBe(true);
  return (await r.json()) as { layers: { name: string; cels?: { items: unknown[] }[] }[] };
};

const items = (p: { layers: { cels?: { items: unknown[] }[] }[] }): number =>
  p.layers.reduce((n, l) => n + (l.cels ?? []).reduce((m, c) => m + c.items.length, 0), 0);

/** A new project from the list's dialog; its id, from the address it opens at. */
const newProject = async (page: Page, name: string): Promise<string> => {
  await page.goto("/");
  await page.getByRole("button", { name: "New project" }).click();
  const dialog = page.getByRole("dialog");
  await dialog.getByLabel("Name").fill(name);
  await dialog.getByRole("button", { name: "Create" }).click();
  await page.waitForURL(/#\/p\/[0-9a-f-]+$/);
  const id = page.url().split("#/p/")[1];
  expect(id).toBeTruthy();
  return id ?? "";
};

/** Waits until the editor says the latest change reached the server. */
const saved = async (page: Page): Promise<void> => {
  await expect(page.locator(".save-state")).toHaveAttribute("data-state", "saved", { timeout: 10_000 });
};

/** A stroke dragged across the middle of the paper. */
const drawStroke = async (page: Page): Promise<void> => {
  const box = await page.locator(".stage").boundingBox();
  expect(box).not.toBeNull();
  if (box === null) return;
  const y = box.y + box.height / 2;
  await page.mouse.move(box.x + box.width * 0.3, y);
  await page.mouse.down();
  for (let i = 1; i <= 12; i++) await page.mouse.move(box.x + box.width * (0.3 + i * 0.03), y + Math.sin(i / 2) * 20);
  await page.mouse.up();
};

test("a new project opens in the editor, named", async ({ page }) => {
  await newProject(page, "E2E opens");
  await expect(page.getByRole("textbox", { name: "Project name" })).toHaveValue("E2E opens");
  await expect(page.locator(".counter")).toContainText("1 / 1");
});

test("a stroke drawn with the mouse is saved to the server, and undo takes it back", async ({ page, request }) => {
  const id = await newProject(page, "E2E stroke");
  await page.keyboard.press("b");
  await drawStroke(page);
  await saved(page);
  expect(items(await stored(request, id))).toBe(1);

  await page.keyboard.press("Control+z");
  await saved(page);
  expect(items(await stored(request, id))).toBe(0);

  await page.keyboard.press("Control+Shift+z");
  await saved(page);
  expect(items(await stored(request, id))).toBe(1);
});

test("a drawing layer is added from the timeline", async ({ page, request }) => {
  const id = await newProject(page, "E2E layers");
  await page.getByRole("button", { name: "Layer", exact: true }).click();
  await page.getByText("Drawing layer", { exact: true }).click();
  await expect(page.locator(".tl-name")).toHaveCount(2);
  await saved(page);
  expect((await stored(request, id)).layers).toHaveLength(2);
});

test("the arrow keys step through the film and stop at its end", async ({ page }) => {
  await newProject(page, "E2E frames");
  await page.keyboard.press("n");
  await expect(page.locator(".counter")).toContainText("/ 2");
  await page.keyboard.press("Home");
  await page.keyboard.press("ArrowRight");
  await page.keyboard.press("ArrowRight");
  await page.keyboard.press("ArrowRight");
  await expect(page.locator(".counter b")).toHaveText("2");
});

test("the reference board opens with W and closes", async ({ page }) => {
  await newProject(page, "E2E board");
  const board = page.getByRole("region", { name: "Reference board" });
  await expect(board).toBeHidden();
  await page.keyboard.press("w");
  await expect(board).toBeVisible();
  await expect(board).toContainText("Add a picture or video");
  await board.getByRole("button", { name: "Close the board" }).click();
  await expect(board).toBeHidden();
});

test("the export dialog offers the presets", async ({ page }) => {
  await newProject(page, "E2E export");
  await page.getByRole("button", { name: "Export" }).click();
  const dialog = page.locator("dialog.export");
  await expect(dialog).toBeVisible();
  await expect(dialog).toContainText("YouTube 1080p");
  await expect(dialog).toContainText("Shorts 1080 × 1920");
});
