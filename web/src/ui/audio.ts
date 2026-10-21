/**
 * Audio tracks: decoded once, drawn as waveforms, played in step with the timeline, and
 * scrubbed: moving the playhead plays the frame under it, for lip sync.
 */
import { assetUrl } from "../api";
import type { Project } from "../domain/project";

/** Peaks per second of audio, for the waveform. */
const PEAKS_PER_SECOND = 100;

export class AudioEngine {
  private ctx: AudioContext | undefined;
  private readonly buffers = new Map<string, AudioBuffer>();
  private readonly waves = new Map<string, Float32Array>();
  private readonly loading = new Map<string, Promise<void>>();
  private playing: AudioBufferSourceNode[] = [];

  private context(): AudioContext {
    this.ctx ??= new AudioContext();
    return this.ctx;
  }

  /** Fetch and decode a track's audio; the waveform is ready once this resolves. */
  load(asset: string): Promise<void> {
    const known = this.loading.get(asset);
    if (known !== undefined) return known;
    const p = (async () => {
      try {
        const res = await fetch(assetUrl(asset), { credentials: "same-origin" });
        if (!res.ok) return;
        const buffer = await this.context().decodeAudioData(await res.arrayBuffer());
        this.buffers.set(asset, buffer);
        this.waves.set(asset, peaks(buffer));
      } catch {
        // Undecodable or unreachable: the track stays silent and shows no waveform.
      }
    })();
    this.loading.set(asset, p);
    return p;
  }

  peaks(asset: string): Float32Array | undefined {
    return this.waves.get(asset);
  }

  /** Every unmuted track from frame `t` on. */
  play(p: Project, t: number, speed: number): void {
    this.stop();
    const ctx = this.context();
    void ctx.resume();
    for (const track of p.audio) {
      const buffer = this.buffers.get(track.asset);
      if (buffer === undefined || track.muted) continue;
      const into = (t - track.offset) / p.fps;
      const src = ctx.createBufferSource();
      src.buffer = buffer;
      src.playbackRate.value = speed;
      const gain = ctx.createGain();
      gain.gain.value = track.volume;
      src.connect(gain).connect(ctx.destination);
      if (into >= 0) src.start(0, Math.min(into, buffer.duration));
      else src.start(ctx.currentTime - into / speed);
      this.playing.push(src);
    }
  }

  /** One frame of sound at `t`, as the playhead is dragged. */
  snippet(p: Project, t: number): void {
    if (p.audio.length === 0) return;
    this.stop();
    const ctx = this.context();
    void ctx.resume();
    for (const track of p.audio) {
      const buffer = this.buffers.get(track.asset);
      const into = (t - track.offset) / p.fps;
      if (buffer === undefined || track.muted || into < 0 || into > buffer.duration) continue;
      const src = ctx.createBufferSource();
      src.buffer = buffer;
      src.connect(ctx.destination);
      src.start(0, into, Math.max(0.06, 1 / p.fps));
      this.playing.push(src);
    }
  }

  stop(): void {
    for (const s of this.playing)
      try {
        s.stop();
      } catch {
        // Already finished.
      }
    this.playing = [];
  }
}

/** Min and max of each slice, interleaved: [min0, max0, min1, max1, …]. */
const peaks = (buffer: AudioBuffer): Float32Array => {
  const data = buffer.getChannelData(0);
  const n = Math.max(1, Math.ceil(buffer.duration * PEAKS_PER_SECOND));
  const per = Math.max(1, Math.floor(data.length / n));
  const out = new Float32Array(n * 2);
  for (let i = 0; i < n; i++) {
    let lo = 0;
    let hi = 0;
    for (let j = i * per; j < Math.min(data.length, (i + 1) * per); j++) {
      const v = data[j] ?? 0;
      if (v < lo) lo = v;
      if (v > hi) hi = v;
    }
    out[i * 2] = lo;
    out[i * 2 + 1] = hi;
  }
  return out;
};
