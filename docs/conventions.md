# Conventions

The house style, on both sides of the wire.

1. **Parse, don't validate.** A domain value is built by its parser and nothing else.
   - Haskell: a `Parse` instance (`src/Anima/Types/Parse.hs`), the type exported
     abstractly, wire instances derived `via Parsed`.
   - TypeScript: a branded type (`web/src/domain/brand.ts`) that only a `parse…` function
     or a smart constructor in `web/src/domain` can produce.
   Holding one is evidence it was checked.
2. **`parse . unparse ≡ Right`** for every type, property-tested (QuickCheck, fast-check),
   with its rejections.
3. **The two sides parse the same shape with the same limits.** `Anima.Types.Project` and
   `web/src/domain/project.ts` change together, and their rejection messages match word for
   word (both spec suites pin one).
4. **No escape hatches in TypeScript.** ESLint refuses `as` casts, `any`, non-null
   assertions and `JSON.parse` outside the two modules whose job they are
   (`domain/brand.ts`, `domain/wire.ts`). Input is `unknown` until a parser has seen it.
5. **Palm rejection is a parse.** A touch pointer cannot become a `PenSample`, so nothing
   downstream can draw with a finger.
6. **Messages say what was expected, never the input.** Nothing user-written reaches a
   log line or an error.
7. **Two values of the same primitive that must never be swapped are two types**
   (`Width`, `Height`).
8. **Closed vocabularies are sums** (`Fps`, `Tool`, `Format`).
9. **Gateways are records of functions** (`ProjectStore`); specs pass their own.
10. **Errors are values** (`Either`, `Result`); only startup throws, naming the flag.
11. **Strict build**: `-Wall -Werror`, explicit export lists, `strict` TypeScript with
    `noUncheckedIndexedAccess` and `exactOptionalPropertyTypes`, typed ESLint. fourmolu,
    hlint and alejandra run as pre-commit hooks.
12. **No task is done until its specs pass** — `nix flake check` runs both suites.
