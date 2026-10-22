# Everything runs inside the flake's dev shell, so the toolchain is the one CI uses.

default:
    @just --list

build:
    nix develop -c cabal build all
    cd web && npm run build

test:
    nix develop -c cabal test --test-show-details=direct
    cd web && npm run check

fmt:
    nix develop -c fourmolu -i app src test

lint:
    nix develop -c hlint app src test
    cd web && npx eslint .

# The API on :9215 with projects in ./data; run `just web` alongside for the editor.
serve:
    nix develop -c cabal run anima2d -- serve --data data --static web/dist

# The editor with hot reload on :5173, talking to `just serve`.
web:
    cd web && npm run dev -- --host

# After changing web/package-lock.json: the hash Nix pins the npm dependencies by.
npm-hash:
    nix develop -c prefetch-npm-deps web/package-lock.json > web/npm-deps.hash

check:
    nix flake check -L

# Build and (re)install the Mac's render agent as a LaunchAgent.
render:
    cd render && npm install && npm run build && ./install.sh

# The example projects and their preview GIFs, drawn by the editor's own renderer on Skia.
examples:
    cd render && npm install && npx esbuild ../examples/build.ts --bundle --platform=node --format=esm --target=node20 --external:@napi-rs/canvas --outfile=dist/examples.mjs --log-level=warning && node dist/examples.mjs ../examples
