import { describe, expect, it } from "vitest";

import { parsePenSample } from "./pen";

const event = (pointerType: string, pressure = 0.7) => ({ pointerType, clientX: 10, clientY: 20, pressure, buttons: 1, timeStamp: 5 });

describe("parsePenSample", () => {
  it("never turns a finger or a palm into ink", () => {
    expect(parsePenSample(event("touch")).ok).toBe(false);
  });

  it("keeps the pencil's pressure", () => {
    const s = parsePenSample(event("pen", 0.31));
    expect(s.ok && s.value.pressure).toBe(0.31);
  });

  it("draws a mouse at a steady middle weight", () => {
    const s = parsePenSample(event("mouse", 0));
    expect(s.ok && s.value.pressure).toBe(0.5);
  });

  it("refuses pressure outside 0 to 1", () => {
    expect(parsePenSample(event("pen", 1.2)).ok).toBe(false);
  });
});
