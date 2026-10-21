import fc from "fast-check";
import { describe, expect, it } from "vitest";

import { parseColor, parseHeight, parseProject, parseProjectId, parseProjectName, parseWidth, unparseProject } from "./project";

const rawPoint = fc.tuple(fc.double({ min: -5000, max: 5000, noNaN: true }), fc.double({ min: -5000, max: 5000, noNaN: true }), fc.double({ min: 0, max: 1, noNaN: true }));

const rawStroke = fc.record({
  tool: fc.constantFrom("pen", "eraser"),
  color: fc.stringMatching(/^#[0-9a-f]{6}$/),
  size: fc.double({ min: 0.5, max: 200, noNaN: true }),
  points: fc.array(rawPoint, { minLength: 1, maxLength: 12 }),
});

const rawProject = fc.record({
  format: fc.constant(1),
  id: fc.stringMatching(/^[a-z0-9-]{1,64}$/),
  name: fc.stringMatching(/^[A-Za-z0-9][A-Za-z0-9 ]{0,78}[A-Za-z0-9]$/),
  canvas: fc.record({ width: fc.integer({ min: 32, max: 3840 }).map((n) => n * 2), height: fc.integer({ min: 32, max: 3840 }).map((n) => n * 2) }),
  fps: fc.constantFrom(12, 24, 25, 30, 48, 50, 60),
  frames: fc.array(fc.record({ hold: fc.integer({ min: 1, max: 24 }), strokes: fc.array(rawStroke, { maxLength: 3 }) }), { minLength: 1, maxLength: 4 }),
});

describe("Project", () => {
  it("parse ∘ unparse is the identity on what parse accepts", () => {
    fc.assert(
      fc.property(rawProject, (raw) => {
        const p = parseProject(raw);
        expect(p.ok).toBe(true);
        if (!p.ok) return;
        expect(parseProject(unparseProject(p.value))).toEqual(p);
      }),
    );
  });

  it("reads the same shape the Haskell server writes", () => {
    fc.assert(
      fc.property(rawProject, (raw) => {
        const p = parseProject(raw);
        if (p.ok) expect(unparseProject(p.value)).toEqual({ ...raw, frames: raw.frames.map((f) => ({ ...f, strokes: f.strokes.map((s) => ({ ...s })) })) });
      }),
    );
  });

  it("rejects an unknown format and an empty timeline", () => {
    fc.assert(
      fc.property(rawProject, (raw) => {
        expect(parseProject({ ...raw, format: 2 }).ok).toBe(false);
        expect(parseProject({ ...raw, frames: [] }).ok).toBe(false);
      }),
    );
  });

  it("names where a nested rejection happened, word for word as the server does", () => {
    const r = parseProject({
      format: 1,
      id: "a",
      name: "A",
      canvas: { width: 1920, height: 1080 },
      fps: 24,
      frames: [{ hold: 1, strokes: [{ tool: "brush", color: "#000000", size: 4, points: [[0, 0, 1]] }] }],
    });
    expect(r).toEqual({ ok: false, error: 'frames: 0: strokes: 0: tool: expected a tool of "pen" or "eraser"' });
  });
});

describe("values", () => {
  it("ProjectId refuses paths and capitals", () => {
    for (const bad of ["../etc", "ABC", "", "a".repeat(65)]) expect(parseProjectId(bad).ok).toBe(false);
  });

  it("ProjectName is trimmed and never repeats a refused input", () => {
    expect(parseProjectName("  Walk cycle ")).toEqual({ ok: true, value: "Walk cycle" });
    const r = parseProjectName("secret".repeat(20));
    expect(r.ok).toBe(false);
    if (!r.ok) expect(r.error).not.toContain("secret");
  });

  it("dimensions are even and in range", () => {
    expect(parseWidth(3840).ok).toBe(true);
    expect(parseHeight(2160).ok).toBe(true);
    for (const n of [1919, 62, 7682, 1.5]) expect(parseWidth(n).ok).toBe(false);
  });

  it("Color is kept lower-case", () => {
    expect(parseColor("#A1B2C3")).toEqual({ ok: true, value: "#a1b2c3" });
    expect(parseColor("#abc").ok).toBe(false);
  });
});
