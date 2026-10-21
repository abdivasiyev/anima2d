import { describe, expect, it } from "vitest";

import { duplicate, insertBlank, newProject, remove } from "./edit";
import { parseCanvas, parseProjectId, parseProjectName } from "./project";
import { unwrap } from "./result";

const blank = () =>
  newProject(unwrap(parseProjectId("p")), unwrap(parseProjectName("Test")), unwrap(parseCanvas({ width: 1920, height: 1080 })), 24);

describe("edits", () => {
  it("keeps at least one drawing", () => {
    expect(remove(blank(), 0).ok).toBe(false);
  });

  it("inserts and duplicates after the current drawing", () => {
    const two = unwrap(insertBlank(blank(), 0));
    expect(two.frames.length).toBe(2);
    const three = unwrap(duplicate(two, 1));
    expect(three.frames[2]).toBe(three.frames[1]);
    expect(unwrap(remove(three, 0)).frames.length).toBe(2);
  });
});
