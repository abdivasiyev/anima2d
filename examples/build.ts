/**
 * Builds the example projects and their previews: `just examples` (bundled with esbuild
 * from render/, so Skia and the editor's code resolve, then run with this folder's path).
 *
 * Each example is written as a project file (`<name>.anima2d.json`, importable from the
 * project list) after being parsed by the editor's own parser, and rendered to a GIF with
 * the editor's own renderer on Skia, the same way the Mac's render agent draws videos.
 */
import "../render/src/canvas";

import { writeFileSync } from "node:fs";
import { join } from "node:path";

import { parsePreset } from "../web/src/domain/export";
import { parseProject, unparseProject, type Project } from "../web/src/domain/project";
import { renderGif } from "../web/src/editor/frames";

// Where to write: the examples folder, given on the command line (the script runs bundled).
const here = process.argv[2] ?? "examples";

// Item helpers ----------------------------------------------------------------------------

type Raw = Record<string, unknown>;

const ellipse = (color: string, fill: string | null, x: number, y: number, w: number, h: number, extra: Raw = {}): Raw => ({
  kind: "shape",
  layer: "line",
  shape: "ellipse",
  color,
  size: 5,
  opacity: 1,
  fill,
  center: [x, y],
  width: w,
  height: h,
  rotation: 0,
  ...extra,
});

const fill = (color: string, points: readonly (readonly [number, number])[], plane = "color"): Raw => ({ kind: "fill", layer: plane, color, opacity: 1, polygons: [points] });

const stroke = (color: string, size: number, points: readonly (readonly [number, number])[], brush = "pen"): Raw => ({
  kind: "stroke",
  layer: "line",
  brush,
  color,
  size,
  opacity: 1,
  points: points.map(([x, y]) => [x, y, 0.7]),
});

const text = (t: string, x: number, y: number, size: number, color: string, style = "bold", font = "sans-serif"): Raw => ({
  kind: "text",
  layer: "line",
  text: t,
  font,
  style,
  size,
  align: "center",
  color,
  opacity: 1,
  center: [x, y],
  rotation: 0,
});

const look = (id: string, name: string, extra: Raw = {}): Raw => ({ id, name, visible: true, locked: false, opacity: 1, ...extra });

const drawing = (id: string, name: string, cels: readonly Raw[], extra: Raw = {}): Raw => ({ kind: "drawing", ...look(id, name), holdLast: false, cels, ...extra });

const held = (id: string, name: string, items: readonly Raw[], extra: Raw = {}): Raw => drawing(id, name, [{ hold: 1, mark: "key", items }], { holdLast: true, ...extra });

const project = (id: string, name: string, w: number, h: number, layers: readonly Raw[], camera: readonly Raw[] = []): Raw => ({
  format: 3,
  id,
  name,
  canvas: { width: w, height: h },
  fps: 24,
  layers,
  camera,
  audio: [],
});

// 1. Bouncing ball: frame by frame, squash and stretch, on twos --------------------------

const bouncingBall = (): Raw => {
  const W = 1280;
  const H = 720;
  const ground = 600;
  // One bounce, drawn on twos: height above the ground and squash per drawing.
  const poses: readonly (readonly [number, number, number, string])[] = [
    [360, 1.0, 1.0, "key"],
    [300, 0.94, 1.08, "inbetween"],
    [180, 0.88, 1.16, "inbetween"],
    [40, 0.84, 1.24, "breakdown"],
    [0, 1.35, 0.68, "key"],
    [40, 0.84, 1.24, "breakdown"],
    [180, 0.88, 1.16, "inbetween"],
    [300, 0.94, 1.08, "inbetween"],
  ];
  const r = 70;
  const cels = poses.map(([up, sx, sy, mark], i) => {
    const x = 240 + i * 110;
    const y = ground - r * sy - up;
    const shadow = Math.max(0.35, 1 - up / 450);
    return {
      hold: 3,
      mark,
      items: [
        { ...ellipse("#1c1c1e", "#1c1c1e", x, ground + 6, 150 * shadow, 22 * shadow), layer: "color", opacity: 0.18 },
        ellipse("#1c1c1e", "#e5484d", x, y, 2 * r * sx, 2 * r * sy),
        { ...stroke("#ffffff", 8, [[x - r * sx * 0.45, y - r * sy * 0.35], [x - r * sx * 0.1, y - r * sy * 0.62]]), opacity: 0.7 },
      ],
    };
  });
  return project("example-bouncing-ball", "Bouncing ball", W, H, [
    held("floor", "Floor", [fill("#f4efe6", [[0, ground], [W, ground], [W, H], [0, H]]), stroke("#1c1c1e", 4, [[0, ground], [W, ground]])]),
    drawing("ball", "Ball", cels),
  ]);
};

// 2. Sunset: parallax layers, a light, a shadow and rim light, a camera move with tint ---

const sunset = (): Raw => {
  const W = 1280;
  const H = 720;
  const band = (y0: number, y1: number): (readonly [number, number])[] => [[-200, y0], [W + 200, y0], [W + 200, y1], [-200, y1]];
  const hills = (base: number, amp: number, step: number, seed: number): (readonly [number, number])[] => {
    const pts: (readonly [number, number])[] = [[-300, H + 50]];
    for (let x = -300; x <= W + 300; x += step) pts.push([x, base - amp * (0.6 + 0.4 * Math.sin(x / 140 + seed)) * (0.7 + 0.3 * Math.cos(x / 61 + seed * 2))]);
    pts.push([W + 300, H + 50]);
    return pts;
  };
  // A traveller walking left to right: the body bobs, the legs swing, on twos.
  const walker = Array.from({ length: 12 }, (_, i) => {
    const t = (i / 12) * Math.PI * 2;
    const x = 520 + i * 9;
    const y = 520 - Math.abs(Math.sin(t)) * 8;
    const swing = Math.sin(t) * 22;
    return {
      hold: 2,
      mark: i % 3 === 0 ? "key" : "inbetween",
      items: [
        stroke("#2a2140", 12, [[x, y + 40], [x - swing, y + 105]]),
        stroke("#2a2140", 12, [[x, y + 40], [x + swing, y + 105]]),
        ellipse("#2a2140", "#3d6fb0", x, y, 70, 100),
        ellipse("#2a2140", "#f2c6a0", x + 4, y - 72, 52, 52),
        stroke("#2a2140", 10, [[x + 10, y - 10], [x + 30 + swing * 0.6, y + 40]]),
      ],
    };
  });
  return project(
    "example-sunset",
    "Sunset walk (parallax and light)",
    W,
    H,
    [
      held("sky", "Sky", [fill("#f6a86b", band(-200, 260)), fill("#f7c48e", band(260, 420)), fill("#fbe2b6", band(420, 900))], { depth: 12 }),
      { kind: "light", ...look("sun", "Sun", { blend: "add", depth: 12 }), light: { shape: "radial", color: "#fff1c4", intensity: 0.85, center: [900, 330], radius: 380, angle: 0, softness: 0.8 } },
      held("mountains", "Mountains", [fill("#b07a8c", hills(430, 160, 40, 1))], { depth: 6 }),
      held("hills", "Hills", [fill("#7f5f7a", hills(520, 90, 30, 4))], { depth: 3 }),
      drawing("traveller", "Traveller", walker, {
        holdLast: true,
        effects: { shadow: { color: "#2a2140", opacity: 0.35, offset: [-26, 14], blur: 10 }, rim: { color: "#fff0c0", opacity: 0.9, width: 5, angle: -0.35 } },
      }),
      held("ground", "Ground", [fill("#4d3b55", [[-300, 625], [W + 300, 610], [W + 300, H + 80], [-300, H + 80]])]),
      held("grass", "Grass", [{ kind: "scatter", layer: "line", brush: "grass", color: "#2c2133", size: 30, opacity: 1, seed: 7, points: [[-200, 716, 0.6], [400, 712, 0.7], [900, 718, 0.6], [1500, 714, 0.7]] }], { depth: 0.5 }),
    ],
    [
      { frame: 0, center: [560, 380], zoom: 1, rotation: 0, ease: "inOut", aperture: 1.5, focus: 1, tint: "#ff9a5c", tintAmount: 0.05 },
      { frame: 23, center: [700, 400], zoom: 1.2, rotation: 0, ease: "linear", aperture: 1.5, focus: 1, tint: "#d9673f", tintAmount: 0.3 },
    ],
  );
};

// 3. Title card: text animated with layer keyframes, a typewriter line, a light sweep ----

const titleCard = (): Raw => {
  const W = 1280;
  const H = 720;
  const subtitle = "Draw it. Time it. Ship it.";
  const cuts = [...subtitle].map((c, i) => (c === " " ? -1 : i + 1)).filter((n) => n > 0);
  const typed = cuts.map((n, k) => ({ hold: k === cuts.length - 1 ? 24 : 1, mark: k === 0 ? "key" : "none", items: [text(subtitle.slice(0, n), W / 2, 470, 44, "#3a3a44", "normal")] }));
  return project("example-title-card", "Title card (keyframes and text)", W, H, [
    held("paper", "Paper", [fill("#f7f5f0", [[0, 0], [W, 0], [W, H], [0, H]])]),
    held("title", "Title", [text("anima2D", W / 2, 330, 160, "#2f6bff"), stroke("#2f6bff", 10, [[420, 430], [640, 445], [860, 430]], "marker")], {
      motion: {
        pivot: [W / 2, 360],
        keys: [
          { frame: 0, position: [0, 40], scale: 0.6, rotation: -0.12, opacity: 0, ease: "out" },
          { frame: 14, position: [0, 0], scale: 1.04, rotation: 0.02, opacity: 1, ease: "inOut" },
          { frame: 20, position: [0, 0], scale: 1, rotation: 0, opacity: 1, ease: "linear" },
        ],
      },
      effects: { shadow: { color: "#1c1c1e", opacity: 0.18, offset: [8, 12], blur: 10 }, rim: null },
    }),
    drawing("subtitle", "Subtitle", [{ hold: 18, mark: "none", items: [] }, ...typed]),
    {
      kind: "light",
      ...look("sweep", "Light sweep", {
        blend: "screen",
        motion: {
          pivot: [W / 2, H / 2],
          keys: [
            { frame: 10, position: [-900, 0], scale: 1, rotation: 0, opacity: 1, ease: "inOut" },
            { frame: 40, position: [900, 0], scale: 1, rotation: 0, opacity: 1, ease: "linear" },
          ],
        },
      }),
      light: { shape: "linear", color: "#ffffff", intensity: 0.6, center: [W / 2, H / 2], radius: 140, angle: 0.35, softness: 0.9 },
    },
  ]);
};

// Build -----------------------------------------------------------------------------------

const examples: readonly (readonly [string, () => Raw])[] = [
  ["bouncing-ball", bouncingBall],
  ["sunset-walk", sunset],
  ["title-card", titleCard],
];

const preview = parsePreset({ id: "preview", name: "Preview", width: 480, height: 270, fps: 12, codec: "h264", quality: "standard", fit: "contain", camera: true, audio: false });
if (!preview.ok) throw new Error(preview.error);

for (const [name, make] of examples) {
  const parsed = parseProject(make());
  if (!parsed.ok) throw new Error(`${name}: ${parsed.error}`);
  const p: Project = parsed.value;
  writeFileSync(join(here, `${name}.anima2d.json`), `${JSON.stringify(unparseProject(p), null, 1)}\n`);
  const gif = await renderGif(p, preview.value, () => undefined, new AbortController().signal);
  if (!gif.ok) throw new Error(`${name}: ${gif.error}`);
  writeFileSync(join(here, "media", `${name}.gif`), new Uint8Array(await gif.value.arrayBuffer()));
  console.log(`${name}: ${p.layers.length} layers, written`);
}
