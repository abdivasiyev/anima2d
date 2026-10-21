/**
 * Brands: the TypeScript half of "parse, don't validate".
 *
 * `Brand<number, "Pressure">` is a number the compiler will not accept where a plain
 * number was given, so the only way to hold one is through the parser that applied it.
 * This module is the one place a cast is allowed (see eslint.config.js), and only
 * src/domain may import it.
 */
declare const brand: unique symbol;

export type Brand<T, B extends string> = T & { readonly [brand]: B };

/** Apply a brand. Call it only after the value has been checked, and only in a parser. */
export const brandChecked = <T, B extends string>(value: T): Brand<T, B> => value as Brand<T, B>;
