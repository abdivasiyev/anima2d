/**
 * The editor's own settings: brush, stabiliser and onion skin. Remembered per device in
 * local storage, and parsed back like anything else read from outside the program.
 */
import { brandChecked, type Brand } from "./brand";
import { andThen, at, err, map, ok, type Result } from "./result";
import { integer, number, object } from "./wire";
import { parseBrushSize, parseColor, parseTool, type BrushSize, type Color, type Tool } from "./project";

/** How hard the line is pulled back toward its smoothed path: 0 (off) to 0.95. */
export type Stabilizer = Brand<number, "Stabilizer">;

export const parseStabilizer = (raw: unknown): Result<Stabilizer> =>
  andThen(number(raw), (n) => (n >= 0 && n <= 0.95 ? ok(brandChecked<number, "Stabilizer">(n)) : err("expected a stabiliser from 0 to 0.95")));

/** Drawings shown before and after the current one: 0 to 5 each way. */
export type OnionRange = Brand<number, "OnionRange">;

export const parseOnionRange = (raw: unknown): Result<OnionRange> =>
  andThen(integer(raw), (n) => (n >= 0 && n <= 5 ? ok(brandChecked<number, "OnionRange">(n)) : err("expected an onion skin range from 0 to 5")));

interface SettingsFields {
  readonly tool: Tool;
  readonly color: Color;
  readonly size: BrushSize;
  readonly stabilizer: Stabilizer;
  readonly onion: boolean;
  readonly onionBefore: OnionRange;
  readonly onionAfter: OnionRange;
}

export type Settings = Brand<SettingsFields, "Settings">;

const bool = (raw: unknown): Result<boolean> => (typeof raw === "boolean" ? ok(raw) : err("expected true or false"));

export const parseSettings = (raw: unknown): Result<Settings> =>
  andThen(object(raw), (o) =>
    andThen(at("tool", parseTool(o.tool)), (tool) =>
      andThen(at("color", parseColor(o.color)), (color) =>
        andThen(at("size", parseBrushSize(o.size)), (size) =>
          andThen(at("stabilizer", parseStabilizer(o.stabilizer)), (stabilizer) =>
            andThen(at("onion", bool(o.onion)), (onion) =>
              andThen(at("onionBefore", parseOnionRange(o.onionBefore)), (onionBefore) =>
                map(at("onionAfter", parseOnionRange(o.onionAfter)), (onionAfter) =>
                  brandChecked<SettingsFields, "Settings">({
                    tool,
                    color,
                    size,
                    stabilizer,
                    onion,
                    onionBefore,
                    onionAfter,
                  }),
                ),
              ),
            ),
          ),
        ),
      ),
    ),
  );

export const unparseSettings = (s: Settings): unknown => ({ ...s });

/** Replace one field with an already-parsed value. */
export const withSetting = <K extends keyof SettingsFields>(s: Settings, key: K, value: SettingsFields[K]): Settings => {
  const next: SettingsFields = { ...s, [key]: value };
  return brandChecked<SettingsFields, "Settings">(next);
};

export const DEFAULT_SETTINGS: unknown = {
  tool: "pen",
  color: "#1c1c1e",
  size: 6,
  stabilizer: 0.4,
  onion: true,
  onionBefore: 2,
  onionAfter: 1,
};
