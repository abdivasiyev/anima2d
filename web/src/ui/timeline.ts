/**
 * The timeline: a ruler to scrub, one row per layer (top layer first, as they stack),
 * then the camera and the audio tracks. A cel is as wide as it is held, so timing reads
 * at a glance; its mark (key, breakdown, inbetween) sits on its top edge.
 *
 * It owns no state: the editor hands it a model and gets intentions back.
 */
import { celStart, duration, isDrawing, type Cel, type Project } from "../domain/project";
import { h } from "./dom";
import { icon } from "./icons";

export interface LoopRange {
  readonly from: number;
  /** Inclusive. */
  readonly to: number;
}

export interface TimelineModel {
  readonly project: Project;
  readonly layer: number;
  readonly cel: number;
  readonly t: number;
  /** Shift-click selects a run of cels on the current layer. */
  readonly rangeEnd: number | undefined;
  readonly loop: LoopRange | undefined;
  readonly pinned: ReadonlySet<Cel>;
}

export interface TimelineEvents {
  selectCel(layer: number, cel: number, extend: boolean): void;
  scrub(t: number): void;
  setLoop(loop: LoopRange | undefined): void;
  toggleVisible(layer: number): void;
  toggleLock(layer: number): void;
  layerMenu(layer: number, x: number, y: number): void;
  /** The menu for the selected cel, at a screen position. */
  celMenu(x: number, y: number): void;
  cameraKeyMenu(frame: number, x: number, y: number): void;
  audioMenu(track: number, x: number, y: number): void;
  thumbnail(cel: Cel, w: number, h: number): OffscreenCanvas;
  peaks(asset: string): Float32Array | undefined;
}

const HEADER = 148;
const ROW = 46;

export class Timeline {
  readonly el: HTMLElement;
  private unit = 28;
  private model: TimelineModel | undefined;
  private playheads: HTMLElement[] = [];
  private readonly ruler: HTMLCanvasElement;
  private readonly rows: HTMLElement;
  private readonly body: HTMLElement;

  constructor(private readonly on: TimelineEvents) {
    this.ruler = h("canvas", { class: "ruler", height: 24 });
    this.rows = h("div", { class: "rows" });
    this.body = h("div", { class: "tl-body" }, h("div", { class: "tl-ruler-row" }, h("div", { class: "tl-corner" }), this.ruler), this.rows);
    this.el = h("div", { class: "tl" }, this.body);

    // Scrub by dragging along the ruler; drag a loop handle to change the loop.
    let dragging: "scrub" | "from" | "to" | undefined;
    const frameAt = (e: PointerEvent): number => {
      const r = this.ruler.getBoundingClientRect();
      return Math.max(0, Math.floor((e.clientX - r.left) / this.unit));
    };
    this.ruler.addEventListener("pointerdown", (e) => {
      const f = frameAt(e);
      const loop = this.model?.loop;
      dragging = loop !== undefined && Math.abs(f - loop.from) <= 0 ? "from" : loop !== undefined && Math.abs(f - loop.to) <= 0 ? "to" : "scrub";
      try {
        this.ruler.setPointerCapture(e.pointerId);
      } catch {
        // The pointer is already gone.
      }
      if (dragging === "scrub") this.on.scrub(f);
    });
    this.ruler.addEventListener("pointermove", (e) => {
      if (dragging === undefined) return;
      const f = frameAt(e);
      const loop = this.model?.loop;
      if (dragging === "scrub") this.on.scrub(f);
      else if (loop !== undefined) this.on.setLoop(dragging === "from" ? { from: Math.min(f, loop.to), to: loop.to } : { from: loop.from, to: Math.max(f, loop.from) });
    });
    this.ruler.addEventListener("pointerup", () => {
      dragging = undefined;
    });
    // Ctrl/⌘ + wheel zooms the timeline itself.
    this.body.addEventListener(
      "wheel",
      (e) => {
        if (!(e.ctrlKey || e.metaKey)) return;
        e.preventDefault();
        this.unit = Math.max(6, Math.min(48, this.unit * Math.exp(-e.deltaY * 0.004)));
        if (this.model !== undefined) this.update(this.model);
      },
      { passive: false },
    );
  }

  update(m: TimelineModel): void {
    this.model = m;
    const p = m.project;
    const total = duration(p) + 24;
    const width = total * this.unit;
    this.drawRuler(m, width);

    const rows: HTMLElement[] = [];
    this.playheads = [];
    // Top layer first, as they stack on the canvas.
    for (let i = p.layers.length - 1; i >= 0; i--) {
      const layer = p.layers[i];
      if (layer === undefined) continue;
      const eye = h("button", { type: "button", class: "tl-toggle", "aria-pressed": String(!layer.visible), title: layer.visible ? "Hide layer" : "Show layer" }, icon(layer.visible ? "eye" : "eyeOff"));
      eye.addEventListener("click", () => {
        this.on.toggleVisible(i);
      });
      const lock = h("button", { type: "button", class: "tl-toggle", "aria-pressed": String(layer.locked), title: layer.locked ? "Unlock layer" : "Lock layer" }, icon(layer.locked ? "lock" : "unlock"));
      lock.addEventListener("click", () => {
        this.on.toggleLock(i);
      });
      const title = h("button", { type: "button", class: "tl-name" }, layer.kind === "reference" ? icon("image") : null, h("span", {}, layer.name));
      title.addEventListener("click", () => {
        if (isDrawing(layer)) this.on.selectCel(i, Math.min(m.layer === i ? m.cel : 0, layer.cels.length - 1), false);
      });
      title.addEventListener("contextmenu", (e) => {
        e.preventDefault();
        this.on.layerMenu(i, e.clientX, e.clientY);
      });
      const more = h("button", { type: "button", class: "tl-more", "aria-label": `${layer.name} options` }, "⋯");
      more.addEventListener("click", (e) => {
        this.on.layerMenu(i, e.clientX, e.clientY);
      });
      const head = h("div", { class: "tl-head", "aria-current": String(i === m.layer) }, eye, lock, title, more);
      const lane = h("div", { class: "tl-lane" });
      lane.style.width = `${width}px`;
      if (isDrawing(layer)) {
        let start = 0;
        for (const [c, cel] of layer.cels.entries()) {
          const w = cel.hold * this.unit;
          const selected = i === m.layer && (c === m.cel || (m.rangeEnd !== undefined && c >= Math.min(m.cel, m.rangeEnd) && c <= Math.max(m.cel, m.rangeEnd)));
          const cell = h("button", {
            type: "button",
            class: `cel mark-${cel.mark}${cel.items.length === 0 ? " empty" : ""}`,
            "aria-current": String(i === m.layer && c === m.cel),
            "aria-selected": String(selected),
            "aria-label": `${layer.name}, drawing ${c + 1}`,
            title: `Drawing ${c + 1} · ${cel.hold} ${cel.hold === 1 ? "frame" : "frames"}${cel.mark === "none" ? "" : ` · ${cel.mark}`}`,
          });
          cell.style.left = `${start * this.unit}px`;
          cell.style.width = `${w}px`;
          if (w >= 26 && cel.items.length > 0) {
            const th = ROW - 12;
            const tw = Math.max(8, w - 6);
            const c2 = h("canvas", { width: tw * 2, height: th * 2 });
            c2.style.width = `${tw}px`;
            c2.style.height = `${th}px`;
            const ctx = c2.getContext("2d");
            ctx?.drawImage(this.on.thumbnail(cel, tw, th), 0, 0);
            cell.append(c2);
          }
          if (m.pinned.has(cel)) cell.append(h("span", { class: "pin", title: "On the light table" }));
          cell.addEventListener("click", (e) => {
            this.on.selectCel(i, c, e.shiftKey);
          });
          cell.addEventListener("contextmenu", (e) => {
            e.preventDefault();
            this.on.selectCel(i, c, false);
            this.on.celMenu(e.clientX, e.clientY);
          });
          lane.append(cell);
          start += cel.hold;
        }
        if (layer.holdLast) {
          const tail = h("div", { class: "hold-tail", title: "Holds its last drawing to the end" });
          tail.style.left = `${start * this.unit}px`;
          tail.style.width = `${Math.max(0, (total - start) * this.unit)}px`;
          lane.append(tail);
        }
      } else {
        const band = h("div", { class: "ref-band" }, layer.media === "video" ? "Reference video" : "Reference image");
        band.style.width = `${width}px`;
        lane.append(band);
      }
      lane.append(this.playhead(m.t));
      rows.push(h("div", { class: "tl-row" }, head, lane));
    }

    // Camera.
    if (p.camera.length > 0) {
      const lane = h("div", { class: "tl-lane slim" });
      lane.style.width = `${width}px`;
      for (const k of p.camera) {
        const d = h("button", { type: "button", class: "cam-key", title: `Camera key at frame ${k.frame + 1}` });
        d.style.left = `${k.frame * this.unit + this.unit / 2 - 6}px`;
        d.addEventListener("click", () => {
          this.on.scrub(k.frame);
        });
        d.addEventListener("contextmenu", (e) => {
          e.preventDefault();
          this.on.cameraKeyMenu(k.frame, e.clientX, e.clientY);
        });
        lane.append(d);
      }
      lane.append(this.playhead(m.t));
      rows.push(h("div", { class: "tl-row slim" }, h("div", { class: "tl-head" }, h("span", { class: "tl-label" }, icon("camera"), "Camera")), lane));
    }

    // Audio.
    for (const [ai, track] of p.audio.entries()) {
      const lane = h("div", { class: "tl-lane slim" });
      lane.style.width = `${width}px`;
      const peaks = this.on.peaks(track.asset);
      if (peaks !== undefined) {
        const fps = p.fps;
        const wave = h("canvas", { class: "wave", height: 56 });
        const frames = peaks.length / 2 / (100 / fps);
        const w = Math.max(1, Math.ceil(frames * this.unit));
        wave.width = Math.min(16000, w);
        wave.style.width = `${w}px`;
        wave.style.left = `${track.offset * this.unit}px`;
        wave.style.opacity = track.muted ? "0.35" : "1";
        const ctx = wave.getContext("2d");
        if (ctx !== null) {
          ctx.fillStyle = getComputedStyle(document.documentElement).getPropertyValue("--accent").trim() || "#2f6bff";
          const n = peaks.length / 2;
          for (let x = 0; x < wave.width; x++) {
            const i = Math.floor((x / wave.width) * n);
            const lo = peaks[i * 2] ?? 0;
            const hi = peaks[i * 2 + 1] ?? 0;
            ctx.fillRect(x, 28 - hi * 26, 1, Math.max(1, (hi - lo) * 26));
          }
        }
        lane.append(wave);
      }
      lane.append(this.playhead(m.t));
      const name = h("button", { type: "button", class: "tl-name" }, icon("music"), h("span", {}, track.name));
      name.addEventListener("click", (e) => {
        this.on.audioMenu(ai, e.clientX, e.clientY);
      });
      rows.push(h("div", { class: "tl-row slim" }, h("div", { class: "tl-head" }, name), lane));
    }

    this.rows.replaceChildren(...rows);
    this.follow(m.t);
  }

  /**
   * Only move the playhead, for playback: rebuilding every row, thumbnail and waveform
   * each frame is far too slow for 24 frames a second.
   */
  seek(t: number): void {
    if (this.model === undefined) return;
    this.model = { ...this.model, t };
    for (const ph of this.playheads) ph.style.left = `${t * this.unit + this.unit / 2}px`;
    this.drawRuler(this.model, (duration(this.model.project) + 24) * this.unit);
    this.follow(t);
  }

  /** Keep the playhead in view. */
  private follow(t: number): void {
    const x = t * this.unit;
    const view = this.body.clientWidth - HEADER;
    if (x < this.body.scrollLeft || x > this.body.scrollLeft + view - this.unit) this.body.scrollLeft = Math.max(0, x - view / 2);
  }

  private playhead(t: number): HTMLElement {
    const ph = h("div", { class: "playhead" });
    this.playheads.push(ph);
    ph.style.left = `${t * this.unit + this.unit / 2}px`;
    return ph;
  }

  private drawRuler(m: TimelineModel, width: number): void {
    const dpr = window.devicePixelRatio || 1;
    this.ruler.width = Math.min(32000, Math.ceil(width * dpr));
    this.ruler.style.width = `${width}px`;
    this.ruler.height = 24 * dpr;
    const ctx = this.ruler.getContext("2d");
    if (ctx === null) return;
    const css = getComputedStyle(document.documentElement);
    const text3 = css.getPropertyValue("--text-3").trim() || "#8e8e96";
    const accent = css.getPropertyValue("--accent").trim() || "#2f6bff";
    ctx.scale(dpr, dpr);
    ctx.clearRect(0, 0, width, 24);
    const loop = m.loop;
    if (loop !== undefined) {
      ctx.fillStyle = "rgba(47, 107, 255, 0.14)";
      ctx.fillRect(loop.from * this.unit, 0, (loop.to - loop.from + 1) * this.unit, 24);
      ctx.fillStyle = accent;
      ctx.fillRect(loop.from * this.unit, 0, 3, 24);
      ctx.fillRect((loop.to + 1) * this.unit - 3, 0, 3, 24);
    }
    const fps = m.project.fps;
    const every = this.unit >= 20 ? 1 : this.unit >= 10 ? 2 : 6;
    ctx.fillStyle = text3;
    ctx.font = "10px -apple-system, system-ui, sans-serif";
    const total = Math.ceil(width / this.unit);
    for (let f = 0; f < total; f++) {
      const x = f * this.unit;
      const second = f % fps === 0;
      if (second || f % every === 0) ctx.fillRect(x, second ? 12 : 18, 1, second ? 12 : 6);
      if (second) ctx.fillText(`${f / fps}s`, x + 3, 10);
      else if (every <= 2 && f % (every * 4) === 0) ctx.fillText(String(f + 1), x + 3, 10);
    }
    ctx.fillStyle = accent;
    ctx.fillRect(m.t * this.unit, 0, this.unit, 3);
  }
}

/** The frame cel `cel` of `layer` starts on, for jumping the playhead to it. */
export const startOf = (p: Project, layer: number, cel: number): number => {
  const l = p.layers[layer];
  return l !== undefined && isDrawing(l) ? celStart(l, cel) : 0;
};
