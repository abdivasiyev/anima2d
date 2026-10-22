/**
 * The render agent on the Mac.
 *
 * the server has no GPU; this Mac does. The server queues a render as a file in the
 * projects folder both can see (~/anima2d/projects/renders/queue). This agent picks
 * it up, draws every frame with the editor's own renderer (on Skia), pipes the pixels to
 * ffmpeg's VideoToolbox encoder, and leaves the MP4 in renders/done with its progress in
 * renders/status. One render at a time.
 */
import { registerFont, skiaImages } from "./canvas";

import { spawn } from "node:child_process";
import { existsSync, mkdirSync, readdirSync, readFileSync, renameSync, statSync, unlinkSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";

import { bitrate, keepsAlpha, parsePreset, presetFps, type Preset } from "../../web/src/domain/export";
import { duration, parseProject, type Project } from "../../web/src/domain/project";
import { decodeJson, object } from "../../web/src/domain/wire";
import { andThen } from "../../web/src/domain/result";
import { forgetLayouts, preloadImages, projectImages, setImageLoader } from "../../web/src/editor/media";
import { CelCache, drawOutput } from "../../web/src/editor/output";

const DATA = process.env.ANIMA2D_DATA ?? join(homedir(), "anima2d", "projects");
const FFMPEG = process.env.ANIMA2D_FFMPEG ?? "ffmpeg";
const ROOT = join(DATA, "renders");
const QUEUE = join(ROOT, "queue");
const WORKING = join(ROOT, "working");
const STATUS = join(ROOT, "status");
const DONE = join(ROOT, "done");

for (const d of [QUEUE, WORKING, STATUS, DONE]) mkdirSync(d, { recursive: true });

const asset = (id: string): string => join(DATA, "assets", id);
setImageLoader(skiaImages(asset));
const fonts = new Set<string>();

const log = (msg: string): void => {
  console.log(`${new Date().toISOString()} ${msg}`);
};

interface Job {
  readonly id: string;
  readonly created: string;
  readonly project: Project;
  readonly preset: Preset;
}

const readJob = (path: string): Job | string => {
  const raw = andThen(decodeJson(readFileSync(path, "utf8")), object);
  if (!raw.ok) return raw.error;
  const o = raw.value;
  const project = parseProject(o.project);
  const preset = parsePreset(o.preset);
  if (!project.ok) return `project: ${project.error}`;
  if (!preset.ok) return `preset: ${preset.error}`;
  if (typeof o.id !== "string" || !/^[0-9a-f-]{36}$/.test(o.id)) return "expected a job id";
  return { id: o.id, created: typeof o.created === "string" ? o.created : "", project: project.value, preset: preset.value };
};

const status = (job: { id: string; created: string; project?: Project; preset?: Preset }, state: string, progress: number, message: string): void => {
  const path = join(STATUS, `${job.id}.json`);
  writeFileSync(
    `${path}.part`,
    JSON.stringify({ id: job.id, state, progress: Math.max(0, Math.min(1, progress)), project: job.project?.name ?? "", preset: job.preset?.name ?? "", created: job.created, message }),
  );
  renameSync(`${path}.part`, path);
};

/** ffmpeg's arguments: raw RGBA frames on stdin, each audio track as its own input, mixed. */
const ffmpegArgs = (job: Job, fps: number, seconds: number, out: string): string[] => {
  const { project: p, preset } = job;
  const args = ["-hide_banner", "-loglevel", "error", "-y", "-f", "rawvideo", "-pix_fmt", "rgba", "-s", `${preset.width}x${preset.height}`, "-r", String(fps), "-i", "pipe:0"];
  const tracks = preset.audio ? p.audio.filter((a) => !a.muted && existsSync(join(DATA, "assets", a.asset))) : [];
  for (const a of tracks) args.push("-i", join(DATA, "assets", a.asset));
  const filters = tracks.map((a, i) => {
    const at = a.offset / p.fps;
    const shift = at >= 0 ? `adelay=${Math.round(at * 1000)}:all=1` : `atrim=start=${(-at).toFixed(3)},asetpts=PTS-STARTPTS`;
    return `[${i + 1}:a]volume=${a.volume},${shift}[a${i}]`;
  });
  if (tracks.length > 0) {
    const inputs = tracks.map((_, i) => `[a${i}]`).join("");
    filters.push(`${inputs}amix=inputs=${tracks.length}:normalize=0,atrim=end=${seconds.toFixed(3)}[aout]`);
    args.push("-filter_complex", filters.join(";"), "-map", "0:v", "-map", "[aout]", ...(preset.codec === "prores" ? ["-c:a", "pcm_s16le"] : ["-c:a", "aac", "-b:a", "192k"]));
  }
  if (preset.codec === "prores") {
    // ProRes 4444 keeps the alpha channel (yuva); software, as VideoToolbox's ProRes drops it.
    args.push("-c:v", "prores_ks", "-profile:v", "4444", "-pix_fmt", keepsAlpha(preset) ? "yuva444p10le" : "yuv444p10le", "-vendor", "apl0", "-color_primaries", "bt709", "-color_trc", "bt709", "-colorspace", "bt709", out);
    return args;
  }
  const encoder = preset.codec === "hevc" ? "hevc_videotoolbox" : "h264_videotoolbox";
  args.push("-c:v", encoder, "-b:v", String(bitrate(preset, presetFps(preset, p.fps))), "-pix_fmt", "yuv420p", "-color_primaries", "bt709", "-color_trc", "bt709", "-colorspace", "bt709");
  if (preset.codec === "hevc") args.push("-tag:v", "hvc1");
  args.push("-movflags", "+faststart", out);
  return args;
};

const render = async (job: Job): Promise<void> => {
  const { project: p, preset } = job;
  const fps = presetFps(preset, p.fps);
  const seconds = duration(p) / p.fps;
  const frames = Math.max(1, Math.round(seconds * fps));
  const ext = preset.codec === "prores" ? "mov" : "mp4";
  const part = join(DONE, `${job.id}.part.${ext}`);
  const out = join(DONE, `${job.id}.${ext}`);
  // The project's fonts and pictures, before the first frame.
  for (const f of p.fonts) {
    const key = `${f.family}\n${f.asset}`;
    if (!fonts.has(key) && existsSync(asset(f.asset))) {
      if (registerFont(asset(f.asset), f.family)) fonts.add(key);
      else log(`font ${f.family} could not be read`);
    }
  }
  forgetLayouts();
  await preloadImages(projectImages(p));
  const canvas = new OffscreenCanvas(preset.width, preset.height);
  const ctx = canvas.getContext("2d");
  if (ctx === null) throw new Error("no 2D context");
  const cache = new CelCache(p.canvas.width, p.canvas.height, 32);
  const ff = spawn(FFMPEG, ffmpegArgs(job, fps, seconds, part), { stdio: ["pipe", "ignore", "pipe"] });
  let errors = "";
  ff.stderr.on("data", (d: Buffer) => {
    errors = (errors + d.toString()).slice(-2000);
  });
  // A missing or broken ffmpeg fails this job, not the agent.
  let spawnError: Error | undefined;
  ff.on("error", (e) => {
    spawnError = e;
  });
  ff.stdin.on("error", () => undefined);
  const finished = new Promise<number>((resolve) => ff.on("close", (code) => resolve(code ?? 1)));
  let last = 0;
  for (let i = 0; i < frames; i++) {
    if (spawnError !== undefined) break;
    drawOutput(ctx, p, Math.floor((i * p.fps) / fps), cache, preset.width, preset.height, preset.camera, preset.fit, keepsAlpha(preset) ? "transparent" : "white");
    const pixels = ctx.getImageData(0, 0, preset.width, preset.height).data;
    if (!ff.stdin.write(Buffer.from(pixels.buffer, pixels.byteOffset, pixels.byteLength))) await new Promise((r) => ff.stdin.once("drain", r));
    if (Date.now() - last > 1000) {
      last = Date.now();
      status(job, "rendering", (i + 1) / frames, `Frame ${i + 1} of ${frames}`);
    }
  }
  ff.stdin.end();
  const code = await finished;
  if (spawnError !== undefined) throw new Error(`ffmpeg could not start: ${spawnError.message}`);
  if (code !== 0) throw new Error(`ffmpeg exited with ${code}: ${errors.trim().split("\n").slice(-3).join(" ")}`);
  renameSync(part, out);
};

const next = (): string | undefined => {
  const jobs = readdirSync(QUEUE)
    .filter((n) => n.endsWith(".json"))
    .map((n) => ({ n, t: statSync(join(QUEUE, n)).mtimeMs }))
    .sort((a, b) => a.t - b.t);
  return jobs[0]?.n;
};

const loop = async (): Promise<void> => {
  for (;;) {
    const name = next();
    if (name === undefined) {
      await new Promise((r) => setTimeout(r, 2000));
      continue;
    }
    const working = join(WORKING, name);
    renameSync(join(QUEUE, name), working);
    const job = readJob(working);
    if (typeof job === "string") {
      log(`refused ${name}: ${job}`);
      status({ id: name.replace(/\.json$/, ""), created: "" }, "failed", 0, `The job could not be read: ${job}`);
      unlinkSync(working);
      continue;
    }
    log(`rendering ${job.id}: ${job.project.name} at ${job.preset.name}`);
    status(job, "rendering", 0, "Starting");
    const started = Date.now();
    try {
      await render(job);
      status(job, "done", 1, `Rendered in ${Math.round((Date.now() - started) / 1000)} s`);
      log(`done ${job.id}`);
    } catch (e) {
      const msg = e instanceof Error ? e.message : String(e);
      status(job, "failed", 0, msg.slice(0, 200));
      log(`failed ${job.id}: ${msg}`);
    }
    unlinkSync(working);
  }
};

// Jobs left in working by a stop mid-render go back to the queue.
for (const n of readdirSync(WORKING).filter((x) => x.endsWith(".json"))) renameSync(join(WORKING, n), join(QUEUE, n));

log(`anima2d render agent watching ${QUEUE} (ffmpeg: ${FFMPEG})`);
void loop();
