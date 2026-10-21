// The rule this project lives by: parse, don't validate.
//
// A domain value (src/domain) is a branded type that only its parser can produce, so the
// compiler refuses one built any other way. The lint rules close the remaining escape
// hatches: no `as` casts and no `any` outside the one helper that applies a brand, no
// non-null assertions, and nothing but src/domain may import that helper.
import js from "@eslint/js";
import tseslint from "typescript-eslint";

export default tseslint.config(
  { ignores: ["dist", "node_modules", "public"] },
  js.configs.recommended,
  ...tseslint.configs.strictTypeChecked,
  ...tseslint.configs.stylisticTypeChecked,
  {
    languageOptions: {
      parserOptions: { projectService: true, tsconfigRootDir: import.meta.dirname },
    },
    rules: {
      "@typescript-eslint/consistent-type-assertions": ["error", { assertionStyle: "never" }],
      "@typescript-eslint/no-explicit-any": "error",
      "@typescript-eslint/no-non-null-assertion": "error",
      "@typescript-eslint/switch-exhaustiveness-check": "error",
      "@typescript-eslint/restrict-template-expressions": ["error", { allowNumber: true }],
      "no-restricted-imports": [
        "error",
        { patterns: [{ group: ["**/domain/brand", "./brand"], message: "Only src/domain applies brands; call a parser instead." }] },
      ],
      "no-restricted-syntax": [
        "error",
        { selector: "CallExpression[callee.object.name='JSON'][callee.property.name='parse']", message: "Decode to `unknown` and parse it with src/domain." },
      ],
    },
  },
  {
    // The single place a brand is applied, after its parser has checked the value.
    files: ["src/domain/brand.ts"],
    rules: { "@typescript-eslint/consistent-type-assertions": "off" },
  },
  {
    files: ["src/domain/**/*.ts"],
    rules: { "no-restricted-imports": "off" },
  },
  {
    // The one place raw JSON text is decoded, straight into `unknown`.
    files: ["src/domain/wire.ts"],
    rules: { "no-restricted-syntax": "off" },
  },
  {
    files: ["eslint.config.js", "vite.config.ts"],
    ...tseslint.configs.disableTypeChecked,
  },
);
