/**
 * The reference board: a panel over the workspace with the project's pictures and videos,
 * to look at while drawing. One is shown at a time, zoomed and panned on its own; a video
 * follows the playhead (for timing to it) or plays by itself; the eyedropper takes a colour
 * from it, and Trace puts it under the drawings as a locked layer. The references are the
 * project's (saved with it); where the panel sits is this browser's.
 */
import { assetUrl } from "../api";
import { makeReference, type Media, type Project, type Reference } from "../domain/project";
import { andThen } from "../domain/result";
import { decodeJson, number, object } from "../domain/wire";
import { h } from "./dom";
import { icon, iconButton, setIcon } from "./icons";
import { openMenu } from "./menu";

export interface BoardHooks {
  /** The board changed (added, removed, follow switched): commit it as an edit. */
  readonly change: (references: readonly Reference[]) => void;
  /** Ask for a file of this kind, upload it, and give back the new reference. */
  readonly add: (accept: string, media: Media) => Promise<Reference | undefined>;
  readonly trace: (index: number) => void;
  readonly pickColour: (hex: string) => void;
  readonly say: (text: string) => void;
}

interface View {
  k: number;
  x: number;
  y: number;
}

interface Placement {
  open: boolean;
  left: number;
  top: number;
  width: number;
  height: number;
}

const STORE = "anima2d.referenceBoard";

const loadPlacement = (): Placement => {
  const fallback = { open: false, left: -1, top: 12, width: 340, height: 300 };
  try {
    const stored = localStorage.getItem(STORE);
    if (stored === null) return fallback;
    const o = andThen(decodeJson(stored), object);
    if (!o.ok) return fallback;
    const num = (k: string, d: number): number => {
      const v = number(o.value[k]);
      return v.ok ? v.value : d;
    };
    return { open: o.value.open === true, left: num("left", -1), top: num("top", 12), width: num("width", 340), height: num("height", 300) };
  } catch {
    return fallback;
  }
};

const savePlacement = (p: Placement): void => {
  try {
    localStorage.setItem(STORE, JSON.stringify(p));
  } catch {
    // Private windows and blocked storage: the panel just starts where it starts.
  }
};

export const referenceBoard = (hooks: BoardHooks) => {
  let references: readonly Reference[] = [];
  let chosen = 0;
  let fps = 24;
  let frame = 0;
  let picking = false;
  let placed = false;
  const views = new Map<string, View>();
  const media = new Map<string, HTMLImageElement | HTMLVideoElement>();
  const placement = loadPlacement();

  const addBtn = iconButton("add", "Add a picture or video");
  const closeBtn = iconButton("close", "Close the board");
  const head = h("div", { class: "refboard-head", title: "Drag to move" }, h("span", { class: "refboard-title" }, "References"), h("div", { class: "spacer" }), addBtn, closeBtn);
  const tabs = h("div", { class: "refboard-tabs" });
  const canvas = h("canvas", { class: "refboard-canvas" });
  const ctx = canvas.getContext("2d", { willReadFrequently: true });
  const empty = h("div", { class: "refboard-empty" }, "Add a picture or video to keep in view while you draw.");
  const viewEl = h("div", { class: "refboard-view" }, canvas, empty);
  const playBtn = iconButton("play", "Play the video");
  const scrub = h("input", { type: "range", class: "refboard-scrub", min: 0, max: 1000, value: 0, "aria-label": "Position in the video" });
  const followBtn = iconButton("pin", "Follow the playhead");
  const pickBtn = iconButton("eyedropper", "Take a colour from it");
  const traceBtn = iconButton("trace", "Trace it: put it under the drawings");
  const removeBtn = iconButton("remove", "Remove from the board");
  const videoBar = h("div", { class: "refboard-video" }, followBtn, playBtn, scrub);
  const foot = h("div", { class: "refboard-foot" }, videoBar, h("div", { class: "spacer" }), pickBtn, traceBtn, removeBtn);
  const el = h("section", { class: "refboard", "aria-label": "Reference board" }, head, tabs, viewEl, foot);
  el.hidden = !placement.open;

  const current = (): Reference | undefined => references[chosen];
  const viewOf = (r: Reference): View => {
    const known = views.get(r.asset);
    if (known !== undefined) return known;
    const v = { k: 1, x: 0, y: 0 };
    views.set(r.asset, v);
    return v;
  };

  const elementOf = (r: Reference): HTMLImageElement | HTMLVideoElement => {
    const known = media.get(r.asset);
    if (known !== undefined) return known;
    const m = r.media === "video" ? h("video", { muted: true, playsinline: true, preload: "auto", crossorigin: "anonymous" }) : h("img", { crossorigin: "anonymous" });
    m.src = assetUrl(r.asset);
    for (const ev of ["load", "loadeddata", "seeked", "timeupdate"]) m.addEventListener(ev, () => { draw(); });
    if (m instanceof HTMLVideoElement) {
      m.addEventListener("play", () => {
        setIcon(playBtn, "pause");
        tick();
      });
      m.addEventListener("pause", () => { setIcon(playBtn, "play"); });
    }
    media.set(r.asset, m);
    return m;
  };

  const sizeOf = (m: HTMLImageElement | HTMLVideoElement): [number, number] => (m instanceof HTMLVideoElement ? [m.videoWidth, m.videoHeight] : [m.naturalWidth, m.naturalHeight]);

  /** Where the reference lands in the view: fitted, then this one's own zoom and pan. */
  const layout = (r: Reference, m: HTMLImageElement | HTMLVideoElement): { x: number; y: number; w: number; h: number } | undefined => {
    const [w, hh] = sizeOf(m);
    if (w === 0 || hh === 0) return undefined;
    const cw = canvas.clientWidth;
    const ch = canvas.clientHeight;
    const v = viewOf(r);
    const k = Math.min(cw / w, ch / hh) * v.k;
    return { x: (cw - w * k) / 2 + v.x, y: (ch - hh * k) / 2 + v.y, w: w * k, h: hh * k };
  };

  const draw = (): void => {
    if (el.hidden || ctx === null) return;
    const dpr = window.devicePixelRatio || 1;
    const cw = Math.max(1, canvas.clientWidth);
    const ch = Math.max(1, canvas.clientHeight);
    if (canvas.width !== Math.round(cw * dpr) || canvas.height !== Math.round(ch * dpr)) {
      canvas.width = Math.round(cw * dpr);
      canvas.height = Math.round(ch * dpr);
    }
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    ctx.clearRect(0, 0, cw, ch);
    const r = current();
    empty.hidden = r !== undefined;
    if (r === undefined) return;
    const m = elementOf(r);
    const at = layout(r, m);
    if (at === undefined) return;
    ctx.imageSmoothingQuality = "high";
    ctx.drawImage(m, at.x, at.y, at.w, at.h);
    if (m instanceof HTMLVideoElement && Number.isFinite(m.duration) && m.duration > 0 && document.activeElement !== scrub) scrub.value = String(Math.round((m.currentTime / m.duration) * 1000));
  };

  const tick = (): void => {
    const r = current();
    const m = r === undefined ? undefined : media.get(r.asset);
    if (!(m instanceof HTMLVideoElement) || m.paused) return;
    draw();
    requestAnimationFrame(tick);
  };

  /** A following video shows the playhead's moment. */
  const follow = (): void => {
    const r = current();
    if (r?.media !== "video" || !r.follow) return;
    const m = elementOf(r);
    if (!(m instanceof HTMLVideoElement)) return;
    if (!m.paused) m.pause();
    const want = frame / fps;
    const end = Number.isFinite(m.duration) ? Math.max(0, m.duration - 0.01) : want;
    if (Math.abs(m.currentTime - Math.min(want, end)) > 0.5 / fps && !m.seeking) m.currentTime = Math.min(want, end);
  };

  const renderChrome = (): void => {
    tabs.replaceChildren(
      ...references.map((r, i) => {
        const b = h("button", { type: "button", class: i === chosen ? "refboard-tab on" : "refboard-tab", title: r.name }, icon(r.media === "video" ? "film" : "image"), h("span", {}, r.name));
        b.addEventListener("click", () => {
          chosen = i;
          picking = false;
          renderChrome();
          follow();
          draw();
        });
        return b;
      }),
    );
    tabs.hidden = references.length < 2;
    const r = current();
    foot.hidden = r === undefined;
    videoBar.hidden = r?.media !== "video";
    if (r?.media === "video") {
      followBtn.classList.toggle("on", r.follow);
      followBtn.title = r.follow ? "Following the playhead: click to play it on its own" : "Follow the playhead";
      playBtn.hidden = r.follow;
      scrub.hidden = r.follow;
    }
    pickBtn.classList.toggle("on", picking);
    viewEl.classList.toggle("picking", picking);
  };

  const place = (): void => {
    const parent = el.parentElement;
    // Opened while the page is still being laid out: wait until the workspace has a size.
    if (parent === null || parent.clientWidth === 0) {
      requestAnimationFrame(place);
      return;
    }
    const pw = parent.clientWidth;
    const ph = parent.clientHeight;
    placement.width = Math.max(240, Math.min(placement.width, pw - 24));
    placement.height = Math.max(200, Math.min(placement.height, ph - 24));
    if (placement.left < 0) placement.left = pw - placement.width - 12;
    placement.left = Math.max(0, Math.min(placement.left, pw - 80));
    placement.top = Math.max(0, Math.min(placement.top, ph - 40));
    el.style.left = `${placement.left}px`;
    el.style.top = `${placement.top}px`;
    el.style.width = `${placement.width}px`;
    el.style.height = `${placement.height}px`;
    placed = true;
  };

  const setOpen = (open: boolean): void => {
    el.hidden = !open;
    placement.open = open;
    savePlacement(placement);
    if (open) {
      place();
      renderChrome();
      follow();
      draw();
    }
  };

  // Moving the panel by its header; resizing it by its corner (CSS), remembered.
  head.addEventListener("pointerdown", (e) => {
    if (e.target instanceof Element && e.target.closest("button") !== null) return;
    e.preventDefault();
    const sx = e.clientX;
    const sy = e.clientY;
    const l0 = placement.left;
    const t0 = placement.top;
    head.setPointerCapture(e.pointerId);
    const move = (ev: PointerEvent): void => {
      placement.left = l0 + ev.clientX - sx;
      placement.top = t0 + ev.clientY - sy;
      place();
    };
    const up = (): void => {
      head.removeEventListener("pointermove", move);
      head.removeEventListener("pointerup", up);
      savePlacement(placement);
    };
    head.addEventListener("pointermove", move);
    head.addEventListener("pointerup", up);
  });
  new ResizeObserver(() => {
    if (el.hidden || !placed) return;
    placement.width = el.offsetWidth;
    placement.height = el.offsetHeight;
    savePlacement(placement);
    draw();
  }).observe(el);

  // Zoom with the wheel about the pointer, pan by dragging, double-click to fit.
  viewEl.addEventListener(
    "wheel",
    (e) => {
      const r = current();
      if (r === undefined) return;
      e.preventDefault();
      e.stopPropagation();
      const v = viewOf(r);
      const rect = canvas.getBoundingClientRect();
      const px = e.clientX - rect.left - rect.width / 2;
      const py = e.clientY - rect.top - rect.height / 2;
      const factor = Math.exp(-e.deltaY * (e.ctrlKey ? 0.01 : 0.0025));
      const k = Math.max(0.2, Math.min(40, v.k * factor));
      const f = k / v.k;
      v.x = px - (px - v.x) * f;
      v.y = py - (py - v.y) * f;
      v.k = k;
      draw();
    },
    { passive: false },
  );
  viewEl.addEventListener("dblclick", () => {
    const r = current();
    if (r === undefined) return;
    views.set(r.asset, { k: 1, x: 0, y: 0 });
    draw();
  });
  viewEl.addEventListener("pointerdown", (e) => {
    const r = current();
    if (r === undefined) return;
    e.stopPropagation();
    if (picking && ctx !== null) {
      const rect = canvas.getBoundingClientRect();
      const dpr = canvas.width / Math.max(1, rect.width);
      try {
        const [pr, pg, pb, pa] = ctx.getImageData(Math.floor((e.clientX - rect.left) * dpr), Math.floor((e.clientY - rect.top) * dpr), 1, 1).data;
        if (pa !== undefined && pa > 0 && pr !== undefined && pg !== undefined && pb !== undefined) {
          hooks.pickColour(`#${[pr, pg, pb].map((c) => c.toString(16).padStart(2, "0")).join("")}`);
          picking = false;
          renderChrome();
        }
      } catch {
        hooks.say("This picture's colours cannot be read here");
      }
      return;
    }
    const v = viewOf(r);
    const sx = e.clientX - v.x;
    const sy = e.clientY - v.y;
    viewEl.setPointerCapture(e.pointerId);
    viewEl.classList.add("dragging");
    const move = (ev: PointerEvent): void => {
      v.x = ev.clientX - sx;
      v.y = ev.clientY - sy;
      draw();
    };
    const up = (): void => {
      viewEl.classList.remove("dragging");
      viewEl.removeEventListener("pointermove", move);
      viewEl.removeEventListener("pointerup", up);
    };
    viewEl.addEventListener("pointermove", move);
    viewEl.addEventListener("pointerup", up);
  });
  // The arrow keys on the video's slider move the video, not the playhead; every other key
  // is still the editor's.
  scrub.addEventListener("keydown", (e) => { e.stopPropagation(); });

  addBtn.addEventListener("click", (e) => {
    const r = addBtn.getBoundingClientRect();
    const take = (accept: string, kind: Media) => async (): Promise<void> => {
      const ref = await hooks.add(accept, kind);
      if (ref === undefined) return;
      chosen = references.length;
      hooks.change([...references, ref]);
    };
    openMenu(r.left, r.bottom + 4, [
      { label: "Picture…", action: () => void take("image/*", "image")() },
      { label: "Video…", action: () => void take("video/*", "video")() },
    ]);
    e.stopPropagation();
  });
  closeBtn.addEventListener("click", () => { setOpen(false); });
  playBtn.addEventListener("click", () => {
    const r = current();
    const m = r === undefined ? undefined : elementOf(r);
    if (!(m instanceof HTMLVideoElement)) return;
    if (m.paused) void m.play();
    else m.pause();
  });
  scrub.addEventListener("input", () => {
    const r = current();
    const m = r === undefined ? undefined : elementOf(r);
    if (!(m instanceof HTMLVideoElement) || !Number.isFinite(m.duration)) return;
    m.currentTime = (Number(scrub.value) / 1000) * m.duration;
  });
  followBtn.addEventListener("click", () => {
    const r = current();
    if (r === undefined) return;
    hooks.change(references.map((x, i) => (i === chosen ? makeReference(x.asset, x.media, x.name, !x.follow) : x)));
  });
  pickBtn.addEventListener("click", () => {
    picking = !picking;
    renderChrome();
  });
  traceBtn.addEventListener("click", () => { hooks.trace(chosen); });
  removeBtn.addEventListener("click", () => {
    if (current() === undefined) return;
    hooks.change(references.filter((_, i) => i !== chosen));
  });

  // Open from last time: placed once the editor is on the page.
  if (placement.open) requestAnimationFrame(() => {
    setOpen(true);
  });

  return {
    el,
    toggle: (): void => { setOpen(el.hidden); },
    get open(): boolean {
      return !el.hidden;
    },
    /** The project's board and the playhead, after any change. */
    sync: (p: Project, t: number): void => {
      fps = p.fps;
      frame = t;
      if (p.references !== references) {
        references = p.references;
        chosen = Math.max(0, Math.min(chosen, references.length - 1));
        if (!el.hidden) renderChrome();
      }
      if (el.hidden) return;
      follow();
      draw();
    },
  };
};
