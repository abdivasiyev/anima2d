import { describe, expect, it } from "vitest";

import { addItems, newProject } from "../domain/edit";
import { parseCanvas, parseItem, parseProjectId, parseProjectName, type Item, type Project } from "../domain/project";
import { unwrap, type Result } from "../domain/result";
import { parseSettings } from "../domain/settings";
import { ObjectEraserTool, SelectTool, type ToolHost } from "./tools";

const ellipse = unwrap(
  parseItem({ kind: "shape", layer: "line", shape: "ellipse", color: "#000000", size: 4, opacity: 1, fill: null, center: [600, 300], width: 200, height: 100, rotation: 0 }),
);
const farStroke = unwrap(
  parseItem({ kind: "stroke", layer: "line", brush: "pen", color: "#000000", size: 4, opacity: 1, points: [[100, 100, 0.5], [150, 160, 0.5]] }),
);

const withItems = (items: readonly Item[]): Project => {
  const blank = newProject(unwrap(parseProjectId("p")), unwrap(parseProjectName("Test")), unwrap(parseCanvas({ width: 1920, height: 1080 })), 24);
  return unwrap(addItems(blank, 0, items));
};

/** A host with no screen: edits land in `state`, previews and guides are ignored. */
const fakeHost = (start: Project): ToolHost & { state: { project: Project; selected: number } } => {
  const state = { project: start, selected: 0 };
  return {
    state,
    width: 1920,
    height: 1080,
    project: () => state.project,
    index: () => 0,
    settings: () => parseSettings({ tool: "select" }),
    edit: (r: Result<Project>) => {
      if (r.ok) state.project = r.value;
      return r.ok;
    },
    preview: () => undefined,
    guides: () => undefined,
    say: () => undefined,
    setColor: () => undefined,
    pixel: () => 1,
    shift: () => false,
    lineRaster: () => ({ width: 1, height: 1, walls: new Uint8Array(1), scale: 1 }),
    colorAt: () => undefined,
    selectionChanged: (n) => {
      state.selected = n;
    },
  };
};

const loop = (cx: number, cy: number, rx: number, ry: number) =>
  Array.from({ length: 33 }, (_, i) => ({ x: cx + Math.cos((i / 32) * Math.PI * 2) * rx, y: cy + Math.sin((i / 32) * Math.PI * 2) * ry, pressure: 0.5 }));

describe("SelectTool", () => {
  it("a lasso selects what it encloses and nothing else", () => {
    const host = fakeHost(withItems([farStroke, ellipse]));
    const tool = new SelectTool(host);
    const [first, ...rest] = loop(600, 300, 160, 110);
    if (first === undefined) throw new Error("no loop");
    tool.down(first);
    for (const p of rest) tool.move(p);
    tool.up(rest[rest.length - 1] ?? first);
    expect(host.state.selected).toBe(1);
    expect(tool.count).toBe(1);
  });

  it("dragging a selected item moves it by the drag", () => {
    const host = fakeHost(withItems([ellipse]));
    const tool = new SelectTool(host);
    tool.down({ x: 700, y: 300, pressure: 0.5 });
    tool.move({ x: 750, y: 340, pressure: 0.5 });
    tool.up({ x: 750, y: 340, pressure: 0.5 });
    const moved = host.state.project.frames[0].items[0];
    expect(moved?.kind === "shape" && [moved.center[0], moved.center[1]]).toEqual([650, 340]);
  });

  it("duplicate adds copies and selects them", () => {
    const host = fakeHost(withItems([ellipse]));
    const tool = new SelectTool(host);
    tool.selectAll();
    tool.duplicate();
    expect(host.state.project.frames[0].items.length).toBe(2);
    expect(tool.count).toBe(1);
  });
});

describe("ObjectEraserTool", () => {
  it("removes whole items it touches", () => {
    const host = fakeHost(withItems([farStroke, ellipse]));
    const tool = new ObjectEraserTool(host);
    tool.down({ x: 700, y: 300, pressure: 1 });
    tool.up();
    expect(host.state.project.frames[0].items).toEqual([farStroke]);
  });
});

