/**
 * The outcome of a parse: the value it proved, or what was expected instead.
 *
 * Errors are values. A rejection says what was expected and never repeats the input,
 * because messages end up in the console and on screen.
 */
export type Result<T> = { readonly ok: true; readonly value: T } | { readonly ok: false; readonly error: string };

export const ok = <T>(value: T): Result<T> => ({ ok: true, value });

export const err = <T = never>(error: string): Result<T> => ({ ok: false, error });

export const map = <A, B>(r: Result<A>, f: (a: A) => B): Result<B> => (r.ok ? ok(f(r.value)) : r);

export const andThen = <A, B>(r: Result<A>, f: (a: A) => Result<B>): Result<B> => (r.ok ? f(r.value) : r);

/** Prefix a rejection with where it happened; the path is ours, never the input. */
export const at = <T>(where: string, r: Result<T>): Result<T> => (r.ok ? r : err(`${where}: ${r.error}`));

/** Parse every element, naming the index of the first that fails. */
export const each = <A, B>(xs: readonly A[], f: (a: A) => Result<B>): Result<B[]> => {
  const out: B[] = [];
  for (const [i, x] of xs.entries()) {
    const r = at(String(i), f(x));
    if (!r.ok) return r;
    out.push(r.value);
  }
  return ok(out);
};

/** The value, or a thrown error: for tests and for values the program itself wrote. */
export const unwrap = <T>(r: Result<T>): T => {
  if (r.ok) return r.value;
  throw new Error(r.error);
};
