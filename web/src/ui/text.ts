/**
 * The text tool: tap to write, tap a text to change what it says. The words are typed into
 * a box over the canvas, in the item's font and size; the item itself is made only from
 * what the box holds when it closes, parsed like everything else.
 */
import { makeItem, normalizeAngle, parseTextBody, parseVec, type Item, type TextFields } from "../domain/project";
import { andThen } from "../domain/result";
import { topmostAt, type XY } from "../editor/geometry";
import type { Sample } from "../editor/stabilizer";
import type { ToolController, ToolHost } from "../editor/tools";

/** How the box looks: as the text it writes or changes. */
export interface TextLook {
  readonly text: string;
  readonly font: TextFields["font"];
  readonly style: TextFields["style"];
  readonly size: number;
  readonly align: TextFields["align"];
  readonly color: string;
  readonly rotation: number;
}

/** Opens the box at a point in the layer's space; resolves with what was typed, undefined if dropped. */
export type AskText = (at: XY, look: TextLook) => Promise<string | undefined>;

export const textTool = (host: ToolHost, ask: AskText): ToolController => {
  let open = false;

  const write = (s: Sample): void => {
    const items = host.items();
    const hit = topmostAt(items, s.x, s.y, 6 * host.pixel());
    const found = hit === undefined ? undefined : items[hit];
    const existing = found?.kind === "text" ? { index: hit, item: found } : undefined;
    const st = host.settings();
    const look = existing?.item ?? {
      font: st.font,
      style: st.textStyle,
      size: st.fontSize,
      align: st.textAlign,
      color: st.color,
      opacity: st.opacity,
      center: [s.x, s.y] as const,
      rotation: 0,
      text: "",
    };
    open = true;
    if (existing?.index !== undefined) host.preview([], new Set([existing.index]));
    void ask([look.center[0], look.center[1]], look).then((typed) => {
      open = false;
      host.preview([]);
      if (typed === undefined) return;
      const body = parseTextBody(typed);
      if (existing?.index !== undefined) {
        const at = existing.index;
        // Emptied: the text goes.
        if (!body.ok) {
          host.edit(host.replaceItems(items.filter((_, i) => i !== at)));
          return;
        }
        const next = makeItem({ ...existing.item, text: body.value });
        if (!next.ok) host.say(next.error);
        else host.edit(host.replaceItems(items.map((it, i): Item => (i === at ? next.value : it))));
        return;
      }
      if (!body.ok) return;
      const made = andThen(body, (text) =>
        andThen(parseVec([look.center[0], look.center[1]]), (center) =>
          makeItem({ kind: "text", layer: "line", text, font: st.font, style: st.textStyle, size: st.fontSize, align: st.textAlign, color: st.color, opacity: st.opacity, center, rotation: normalizeAngle(0) }),
        ),
      );
      if (!made.ok) host.say(made.error);
      else host.edit(host.addItems([made.value]));
    });
  };

  return {
    down: (s) => {
      if (!open) write(s);
    },
    move: () => undefined,
    up: () => undefined,
    cancel: () => undefined,
    deactivate: () => undefined,
  };
};
