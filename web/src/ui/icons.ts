import {
  ArrowLeft,
  ChevronLeft,
  ChevronRight,
  Copy,
  createElement,
  Eraser,
  Layers,
  Maximize2,
  Minimize2,
  Pause,
  PenLine,
  Play,
  Plus,
  Redo2,
  Spline,
  Trash2,
  Undo2,
  type IconNode,
} from "lucide";

export const ICONS = {
  back: ArrowLeft,
  prev: ChevronLeft,
  next: ChevronRight,
  duplicate: Copy,
  eraser: Eraser,
  onion: Layers,
  fullscreen: Maximize2,
  exitFullscreen: Minimize2,
  pause: Pause,
  pen: PenLine,
  play: Play,
  add: Plus,
  redo: Redo2,
  stabilizer: Spline,
  remove: Trash2,
  undo: Undo2,
} satisfies Record<string, IconNode>;

export type IconName = keyof typeof ICONS;

export const icon = (name: IconName): SVGElement => createElement(ICONS[name], { "aria-hidden": "true", "stroke-width": 1.75 });

/** The anima2D mark: three onion-skin frames of one stroke. */
export const mark = (): SVGElement => {
  const ns = "http://www.w3.org/2000/svg";
  const svg = document.createElementNS(ns, "svg");
  svg.setAttribute("viewBox", "0 0 24 24");
  svg.setAttribute("fill", "none");
  svg.setAttribute("aria-hidden", "true");
  const paths: readonly [string, string][] = [
    ["M4 17c3-7 6-9 9-9", "0.3"],
    ["M6 18c3-6 6-8.5 10-8.5", "0.6"],
    ["M8 19c3-6 7-8 12-8", "1"],
  ];
  for (const [d, opacity] of paths) {
    const p = document.createElementNS(ns, "path");
    p.setAttribute("d", d);
    p.setAttribute("stroke", "currentColor");
    p.setAttribute("stroke-width", "2.2");
    p.setAttribute("stroke-linecap", "round");
    p.setAttribute("opacity", opacity);
    svg.append(p);
  }
  return svg;
};

/** An icon button with an accessible name and a hover hint. */
export const iconButton = (name: IconName, label: string, shortcut?: string): HTMLButtonElement => {
  const b = document.createElement("button");
  b.type = "button";
  b.className = "icon-btn";
  b.setAttribute("aria-label", label);
  b.title = shortcut === undefined ? label : `${label} (${shortcut})`;
  b.append(icon(name));
  return b;
};

export const setIcon = (b: HTMLButtonElement, name: IconName): void => {
  b.replaceChildren(icon(name));
};
