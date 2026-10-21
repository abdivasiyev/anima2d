# anima2D

A self-hosted 2D animation studio for the browser, made for the iPad and Apple Pencil.
Frame-by-frame drawing with onion skin and a timeline. Projects are saved on your own
server.

- **Pencil-first.** Pressure-sensitive strokes and a line stabiliser. Palm rejection:
  fingers never draw. One finger pans; two fingers pan, zoom and rotate the canvas.
  A two-finger tap undoes and a three-finger tap redoes.
- **Animation.** Drawings on a timeline, each held for 1–24 frames ("on twos"). Onion skin
  shows up to five drawings before (red) and after (green). Playback runs at 12–60 fps.
- **Canvases** at Full HD, 4K, Shorts (9:16) or square. Strokes are stored as vectors (pen
  samples), so they can be re-rendered at any resolution.
- **Fullscreen.** A toolbar button (or `F`) hides the browser's tabs and toolbars. Added
  to the iPad home screen, the app opens fullscreen with no browser UI.
- **Saving.** Every change autosaves to the server a moment after you lift the pencil.

Video export (4K / 2K / 1080p / Shorts presets), rigging and motion paths are on the
roadmap.

## Layout

| Path | What |
|---|---|
| `src/`, `app/`, `test/` | The server, in Haskell: Servant API + the editor's static files |
| `web/` | The editor, in TypeScript (Vite, no UI framework) |
| `docs/conventions.md` | **Parse, don't validate** and the other house rules — read first |
| `flake.nix` | Builds both halves and runs both spec suites (`nix flake check`) |

## Developing

```sh
just serve   # API on 127.0.0.1:9215, projects in ./data
just web     # editor with hot reload on :5173 (proxies /api to the server)
just test    # Haskell specs, then ESLint + vitest
```

`cd web && npm install` once before `just web`. After changing `web/package-lock.json`,
run `just npm-hash` so the Nix build pins the new dependencies.

### Mouse and trackpad

The left button draws. The scroll wheel and a trackpad pinch zoom about the cursor. A
sideways swipe, Shift+scroll, Space+drag or a middle-button drag pans. Right-click opens
anima2D's own menu (the browser's is turned off outside text fields). On the iPad, a
one-finger long-press opens the same menu.

### Keyboard

| Key | Action |
|---|---|
| `B` / `E` | Pen / eraser |
| `[` / `]` | Smaller / larger brush |
| `,` / `.` | Previous / next drawing |
| `N` / `D` | New / duplicate drawing |
| `Space` | Play / pause |
| `O` | Onion skin on / off |
| `⌘Z` / `⇧⌘Z` | Undo / redo |
| `0` | Fit the canvas to the screen |
| `F` | Fullscreen |
| `Delete` | Clear the drawing |

## API

| Method | Path | |
|---|---|---|
| `GET` | `/healthz` | `ok` |
| `GET` | `/api/projects` | Project summaries |
| `GET` | `/api/projects/:id` | One project |
| `PUT` | `/api/projects/:id` | Save a project; the body is parsed with the same rules as the editor |

The server has no login of its own. Put it behind a single sign-on gate. At
example.com, Caddy forwards every request to an OAuth2 proxy first.

## Deploying

A push to `main` runs `.forgejo/workflows/deploy.yaml` on the host's Actions runner. The
workflow builds the flake and hands the store path to the deploy socket set up in the
server repository (`the host configuration`). That socket switches the profile,
restarts the service and rolls back if `/healthz` does not answer.

## Licence

Apache-2.0.
