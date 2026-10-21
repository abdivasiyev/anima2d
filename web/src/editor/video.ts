/**
 * Rendering a video in the browser: every frame through the same output renderer as
 * playback, encoded with WebCodecs (the device's hardware encoder where it has one) and
 * written as an MP4 with the index at the front, as YouTube prefers.
 */
import { AudioBufferSource, BufferTarget, CanvasSource, Mp4OutputFormat, Output, Quality, canEncodeVideo } from "mediabunny";

import { bitrate, presetFps, type Preset } from "../domain/export";
import { duration, type Project } from "../domain/project";
import { err, ok, type Result } from "../domain/result";
import { CelCache, drawOutput } from "./output";

const SAMPLE_RATE = 48000;

/** Every unmuted track mixed into one stereo buffer as long as the video. */
export const mixAudio = async (p: Project, buffers: ReadonlyMap<string, AudioBuffer>, seconds: number): Promise<AudioBuffer | undefined> => {
  const tracks = p.audio.filter((a) => !a.muted && buffers.has(a.asset));
  if (tracks.length === 0) return undefined;
  const ctx = new OfflineAudioContext(2, Math.max(1, Math.ceil(seconds * SAMPLE_RATE)), SAMPLE_RATE);
  for (const a of tracks) {
    const buffer = buffers.get(a.asset);
    if (buffer === undefined) continue;
    const src = ctx.createBufferSource();
    src.buffer = buffer;
    const gain = ctx.createGain();
    gain.gain.value = a.volume;
    src.connect(gain).connect(ctx.destination);
    const at = a.offset / p.fps;
    if (at >= 0) src.start(at);
    else src.start(0, Math.min(-at, buffer.duration));
  }
  return ctx.startRendering();
};

export interface Progress {
  readonly frame: number;
  readonly frames: number;
}

export const renderVideo = async (
  p: Project,
  preset: Preset,
  buffers: ReadonlyMap<string, AudioBuffer>,
  onProgress: (pr: Progress) => void,
  signal: AbortSignal,
): Promise<Result<Blob>> => {
  const fps = presetFps(preset, p.fps);
  const codec = preset.codec === "hevc" ? "hevc" : "avc";
  const rate = new Quality({ bitrate: bitrate(preset, fps), bitrateMode: "variable" });
  if (!(await canEncodeVideo(codec, { width: preset.width, height: preset.height, quality: rate })))
    return err(`this browser cannot encode ${preset.codec.toUpperCase()} at ${preset.width} × ${preset.height}; try Render on the Mac, or a smaller preset`);
  // The timeline is in project frames; a preset at another rate samples it in time.
  const seconds = duration(p) / p.fps;
  const frames = Math.max(1, Math.round(seconds * fps));
  const canvas = new OffscreenCanvas(preset.width, preset.height);
  const ctx = canvas.getContext("2d");
  if (ctx === null) return err("this browser cannot draw off screen");
  const target = new BufferTarget();
  const output = new Output({ format: new Mp4OutputFormat({ fastStart: "in-memory" }), target });
  const video = new CanvasSource(canvas, { codec, quality: rate, keyFrameInterval: 2 });
  output.addVideoTrack(video, { frameRate: fps });
  const mixed = preset.audio ? await mixAudio(p, buffers, seconds) : undefined;
  const sound = mixed === undefined ? undefined : new AudioBufferSource({ codec: "aac", quality: new Quality({ bitrate: 192000 }) });
  if (sound !== undefined) output.addAudioTrack(sound);
  const cache = new CelCache(p.canvas.width, p.canvas.height, 32);
  try {
    await output.start();
    for (let i = 0; i < frames; i++) {
      if (signal.aborted) {
        await output.cancel();
        return err("export cancelled");
      }
      drawOutput(ctx, p, Math.floor((i * p.fps) / fps), cache, preset.width, preset.height, preset.camera, preset.fit);
      await video.add(i / fps, 1 / fps);
      onProgress({ frame: i + 1, frames });
    }
    if (sound !== undefined && mixed !== undefined) await sound.add(mixed);
    await output.finalize();
  } catch (e) {
    return err(`the encoder stopped: ${e instanceof Error ? e.message : "unknown error"}`);
  }
  const bytes = target.buffer;
  return bytes === null ? err("the encoder produced nothing") : ok(new Blob([bytes], { type: "video/mp4" }));
};
