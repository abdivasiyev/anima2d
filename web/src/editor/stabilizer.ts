/**
 * The line stabiliser: a "pulled string". The ink follows the pen at a distance set by
 * the strength, so hand tremor is absorbed and curves come out smooth. When the pen
 * lifts, the ink catches up to where the pen actually stopped.
 */
import type { Stabilizer } from "../domain/settings";

export interface Sample {
  readonly x: number;
  readonly y: number;
  readonly pressure: number;
}

export class Smoother {
  private last: Sample | undefined;

  constructor(private readonly strength: Stabilizer) {}

  /** The smoothed sample for this raw one. */
  push(raw: Sample): Sample {
    const prev = this.last;
    if (prev === undefined) {
      this.last = raw;
      return raw;
    }
    const k = 1 - this.strength;
    const next: Sample = {
      x: prev.x + (raw.x - prev.x) * k,
      y: prev.y + (raw.y - prev.y) * k,
      pressure: prev.pressure + (raw.pressure - prev.pressure) * Math.min(1, k * 1.5),
    };
    this.last = next;
    return next;
  }

  /** Samples that close the gap between the smoothed ink and the pen's final position. */
  finish(target: Sample): Sample[] {
    const out: Sample[] = [];
    if (this.last === undefined || this.strength === 0) return out;
    for (let i = 0; i < 24; i++) {
      const s = this.push({ ...target, pressure: this.last.pressure });
      out.push(s);
      if (Math.hypot(s.x - target.x, s.y - target.y) < 0.25) break;
    }
    return out;
  }
}
