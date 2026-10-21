/**
 * Export presets: the size, rate and quality a video is rendered at.
 *
 * The YouTube sizes are built in; presets of your own are kept on the device and parsed
 * back like anything else read from storage. The server parses a preset the same way
 * before it queues a render (Anima.Renders), and the Mac's render agent parses it again.
 */
import { brandChecked, type Brand } from "./brand";
import { andThen, each, err, ok, type Result } from "./result";
import { FPS_VALUES, parseFps, parseHeight, parseLabel, parseWidth, type Fps, type Height, type LabelName, type Width } from "./project";
import { array, bind, field, object, parseWith, string } from "./wire";

export const CODECS = ["h264", "hevc"] as const;
export type Codec = (typeof CODECS)[number];

export const QUALITIES = ["standard", "high", "max"] as const;
/** Bitrate relative to YouTube's recommendation for the size and rate: 1×, 1.5×, 2.5×. */
export type Quality = (typeof QUALITIES)[number];

export const FITS = ["contain", "cover"] as const;
/** When the preset's shape differs from the canvas: letterbox it, or fill and crop. */
export type Fit = (typeof FITS)[number];

const oneOf = <T extends string>(values: readonly T[], expected: string) => (raw: unknown): Result<T> => {
  const found = values.find((v) => v === raw);
  return found === undefined ? err(expected) : ok(found);
};

const parseBool = (raw: unknown): Result<boolean> => (typeof raw === "boolean" ? ok(raw) : err("expected true or false"));

/** A preset's own frame rate, or "project" to keep the project's. */
export type PresetFps = Fps | "project";

const parsePresetFps = (raw: unknown): Result<PresetFps> => (raw === "project" ? ok("project") : parseFps(raw));

export interface PresetFields {
  readonly id: string;
  readonly name: LabelName;
  readonly width: Width;
  readonly height: Height;
  readonly fps: PresetFps;
  readonly codec: Codec;
  readonly quality: Quality;
  readonly fit: Fit;
  /** Render through the camera keys, if the project has any. */
  readonly camera: boolean;
  readonly audio: boolean;
}

export type Preset = Brand<PresetFields, "Preset">;

export const parsePreset = (raw: unknown): Result<Preset> =>
  parseWith(function* () {
    const o = yield* bind(object(raw));
    const id = yield* field(o, "id", (v) => andThen(string(v), (s) => (/^[a-z0-9-]{1,40}$/.test(s) ? ok(s) : err("expected a preset id of 1 to 40 of a-z, 0-9 and '-'"))));
    return ok(
      brandChecked<PresetFields, "Preset">({
        id,
        name: yield* field(o, "name", parseLabel),
        width: yield* field(o, "width", parseWidth),
        height: yield* field(o, "height", parseHeight),
        fps: yield* field(o, "fps", parsePresetFps),
        codec: yield* field(o, "codec", oneOf(CODECS, 'expected a codec of "h264" or "hevc"')),
        quality: yield* field(o, "quality", oneOf(QUALITIES, 'expected a quality of "standard", "high" or "max"')),
        fit: yield* field(o, "fit", oneOf(FITS, 'expected a fit of "contain" or "cover"')),
        camera: yield* field(o, "camera", parseBool),
        audio: yield* field(o, "audio", parseBool),
      }),
    );
  });

export const unparsePreset = (p: Preset): unknown => ({ ...p });

const builtIn = (id: string, name: string, width: number, height: number): Preset => {
  const r = parsePreset({ id, name, width, height, fps: "project", codec: "h264", quality: "high", fit: "contain", camera: true, audio: true });
  if (!r.ok) throw new Error(`built-in preset ${id}: ${r.error}`);
  return r.value;
};

export const BUILT_IN: readonly Preset[] = [
  builtIn("youtube-4k", "YouTube 4K", 3840, 2160),
  builtIn("youtube-2k", "YouTube 2K (1440p)", 2560, 1440),
  builtIn("youtube-1080", "YouTube 1080p", 1920, 1080),
  builtIn("youtube-720", "YouTube 720p", 1280, 720),
  builtIn("shorts", "Shorts 1080 × 1920", 1080, 1920),
  builtIn("shorts-4k", "Shorts 4K", 2160, 3840),
];

export const parsePresets = (raw: unknown): Result<Preset[]> => andThen(array(raw), (xs) => each(xs, parsePreset));

/** The frame rate a preset renders at. */
export const presetFps = (p: Preset, projectFps: Fps): Fps => (p.fps === "project" ? projectFps : p.fps);

/**
 * Video bitrate in bits per second: YouTube's recommended upload rates (8 Mbps at
 * 1080p30 up to 45 Mbps at 4K30), half again for 48–60 fps, scaled by quality.
 */
export const bitrate = (p: Preset, fps: Fps): number => {
  const pixels = p.width * p.height;
  const base = pixels >= 3840 * 2160 * 0.9 ? 45e6 : pixels >= 2560 * 1440 * 0.9 ? 16e6 : pixels >= 1920 * 1080 * 0.9 ? 8e6 : 5e6;
  const rate = fps >= 48 ? 1.5 : 1;
  const q = p.quality === "standard" ? 1 : p.quality === "high" ? 1.5 : 2.5;
  return Math.round(base * rate * q);
};

export { FPS_VALUES };
