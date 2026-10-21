/**
 * Pointer input, parsed.
 *
 * Palm rejection is a parse: a finger or a palm is a "touch" pointer, and a touch pointer
 * never becomes a `PenSample`, so nothing downstream can draw with one. Fingers are
 * routed to gestures instead (src/input/gestures.ts).
 */
import { brandChecked, type Brand } from "./brand";
import { err, ok, type Result } from "./result";
import { parsePressure, type Pressure } from "./project";

/** Where an input sample came from; only these may draw. */
export type DrawingDevice = "pen" | "mouse";

export type PenSample = Brand<
  {
    readonly device: DrawingDevice;
    readonly clientX: number;
    readonly clientY: number;
    readonly pressure: Pressure;
    /** Milliseconds, from the event's own clock. */
    readonly time: number;
  },
  "PenSample"
>;

/** The fields of a PointerEvent the parser reads; a plain object in the specs. */
export interface PointerLike {
  readonly pointerType: string;
  readonly clientX: number;
  readonly clientY: number;
  readonly pressure: number;
  readonly buttons: number;
  readonly timeStamp: number;
}

/** A mouse reports no pressure; draw it at a steady middle weight. */
const MOUSE_PRESSURE = 0.5;

export const parsePenSample = (e: PointerLike): Result<PenSample> => {
  if (e.pointerType !== "pen" && e.pointerType !== "mouse") return err("expected a pen or a mouse");
  if (!Number.isFinite(e.clientX) || !Number.isFinite(e.clientY)) return err("expected a position on screen");
  const pressure = parsePressure(e.pointerType === "mouse" ? MOUSE_PRESSURE : e.pressure);
  if (!pressure.ok) return pressure;
  return ok(
    brandChecked({
      device: e.pointerType,
      clientX: e.clientX,
      clientY: e.clientY,
      pressure: pressure.value,
      time: e.timeStamp,
    }),
  );
};

/** Whether this event is a finger (or palm) on the screen: gesture input, never ink. */
export const isTouch = (e: PointerLike): boolean => e.pointerType === "touch";
