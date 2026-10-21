import { err, ok, type Result } from "./result";

/**
 * Reading `unknown` JSON one shape at a time. Every parser in src/domain starts here;
 * nothing else in the editor ever looks inside an `unknown`.
 */
export type Fields = Readonly<Record<string, unknown>>;

export const object = (raw: unknown): Result<Fields> => {
  if (typeof raw !== "object" || raw === null || Array.isArray(raw)) return err("expected an object");
  const fields: Record<string, unknown> = {};
  for (const [k, v] of Object.entries(raw)) fields[k] = v;
  return ok(fields);
};

export const array = (raw: unknown): Result<readonly unknown[]> =>
  Array.isArray(raw) ? ok(raw.map((x: unknown) => x)) : err("expected an array");

export const number = (raw: unknown): Result<number> =>
  typeof raw === "number" && Number.isFinite(raw) ? ok(raw) : err("expected a number");

export const integer = (raw: unknown): Result<number> =>
  typeof raw === "number" && Number.isInteger(raw) ? ok(raw) : err("expected a whole number");

export const string = (raw: unknown): Result<string> => (typeof raw === "string" ? ok(raw) : err("expected text"));

export const bounded = <T>(what: string, lo: number, hi: number, xs: readonly T[]): Result<readonly T[]> =>
  xs.length < lo || xs.length > hi ? err(`expected ${lo} to ${hi} ${what}`) : ok(xs);

/** Text that should hold JSON, decoded to `unknown` for a parser to look at. */
export const decodeJson = (text: string): Result<unknown> => {
  try {
    const raw: unknown = JSON.parse(text);
    return ok(raw);
  } catch {
    return err("expected JSON");
  }
};
