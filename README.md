# anima2D

A self-hosted 2D animation studio for the browser, made for the iPad and Apple Pencil.
Draw frame by frame with onion skin and a timeline, colour under your linework, tween and
rig what you would rather not redraw, and export for YouTube and Shorts. Everything lives
on your own server; the editor keeps working when that server is off.

## What it does

**Drawing**
- Pencil-first: pressure and tilt, a line stabiliser, and palm rejection (fingers never
  draw; one finger pans, two pan, zoom and rotate; two- and three-finger taps undo and redo).
- Brushes: pen, pencil (with grain), marker, and paint, which goes under the lines.
- Scatter brushes: grass, birds, stars, leaves, flowers and spray, stamped along the stroke
  (each stroke is seeded, so it renders the same every time).
- Shapes: line, rectangle, ellipse and triangle, outlined or filled, snapping to squares,
  circles and 15° lines. Curves: drag, then bend with a handle.
- Fill bucket that closes small gaps and fills under the lines; eyedropper.
- Pixel eraser and object eraser (removes whole strokes, shapes and fills).
- Selection: tap or lasso; move, scale, rotate, flip, restack, recolour, duplicate, copy
  and paste between drawings.
- Mirror symmetry; hold the pen still at the end of a line to straighten it.
- Text in any of the browser's fonts or in font files added to the project: bold,
  italic, aligned, turned; select it to move and scale it like any item.
- Pictures: a background picture (a layer of its own, held to the end) or a picture in
  any drawing, to move, scale, turn, mirror and animate.
- Project colours kept with the project; recolour every item in one colour at once.
- Guides: title- and action-safe frames and a grid with the thirds marked.

**Animating**
- Layers, bottom to top: drawing layers, background layers (drawn once, held to the end),
  and reference images or videos to trace over.
- A timeline with a row per layer; each drawing is as wide as it is held. Marks for keys,
  breakdowns and inbetweens. Scrub the ruler (audio follows, for lip sync); loop a range,
  play ping-pong or slowed down.
- Onion skin before and after, tinted or in original colours, or keys only; pin any
  drawing to the light table; shift and trace slides the skins to trace a part anew.
- Layer keyframes: the Transform tool moves, scales, turns and fades a whole layer about
  its pivot, keyed at the playhead, with easing; the path shows on the canvas.
- Blend modes per layer (multiply, screen, overlay, add, darken, lighten), and clipping
  a layer to the one below for shading inside the lines.
- Typewriter: a text typed on a letter or a word at a time.
- Sequences: reverse, duplicate, repeat as a cycle, ping-pong, delete.
- Inbetween: drawings between two others, by matching their items (strokes morph,
  shapes interpolate, colours blend), with easing.
- Move along a path: draw a path and a selection travels it, optionally turning with it.
- Camera: frame the shot by panning and zooming, set keys, and playback moves between them.
- Audio tracks with waveforms, offsets and mute.
- Rigging (cut-out): bones on a layer's first drawing, parts bound to bones, poses on the
  other drawings (turn a bone, or Shift to reach with the whole chain); inbetweening two
  poses turns the bones in arcs.

**Saving and exporting**
- Every edit is saved on the device at once and on the server shortly after. With the
  server unreachable the app still opens (a service worker caches it) and edits sync when
  it is back. Saves are revisioned: two devices, or the editor and an agent, never
  overwrite each other; a conflicting version is kept as a copy.
- Export presets for YouTube 4K, 2K, 1080p, 720p and Shorts, and presets of your own.
  Export on the device (WebCodecs MP4, PNG frames in a ZIP, or an animated GIF), or render
  on the Mac through the render agent (H.264 and HEVC on VideoToolbox, or ProRes 4444).
  A transparent background is kept by PNG frames, GIFs and ProRes.

## Layout

| Path | What |
|---|---|
| `src/`, `app/`, `test/` | The server, in Haskell: the projects API, uploads, the render queue, MCP |
| `web/` | The editor, in TypeScript (Vite, no UI framework) |
| `render/` | The Mac render agent (Node, Skia, ffmpeg VideoToolbox) |
| `fixtures/` | Parser cases both test suites run: accept, upgrade and reject alike |
| `docs/conventions.md` | **Parse, don't validate** and the other house rules; read first |

## Developing

```sh
just serve   # API on 127.0.0.1:9215, projects in ./data
just web     # editor with hot reload on :5173 (proxies /api to the server)
just test    # Haskell specs, then ESLint + vitest
```

`cd web && npm install` once before `just web`. After changing `web/package-lock.json`,
run `just npm-hash` so the Nix build pins the new dependencies.

### Keyboard

| Key | Action |
|---|---|
| `V` `Q` `B` `G` `E` `U` `T` `K` `I` `R` | Select, transform, brush, scatter, eraser, shapes, text, fill, eyedropper, rig |
| `[` / `]` | Smaller / larger brush |
| `,` / `.` | Previous / next drawing; `←` / `→` one frame |
| `N` / `D` | New / duplicate drawing |
| `Space` | Play / pause; `L` loops the selection |
| `O` | Onion skin; `P` pins the drawing to the light table; `M` mirror |
| `⌘Z` / `⇧⌘Z` | Undo / redo; `⌘C` `⌘V` `⌘D` `⌘A` on selections |
| `0` / `F` | Fit to screen / fullscreen |

Mouse and trackpad: the left button draws; the wheel and a pinch zoom; Shift+scroll, a
sideways swipe, Space+drag or the middle button pan; right-click (long-press on the iPad)
opens the menu. Drag the toolbar by its grip to either edge; drag the timeline's top edge
to resize it.

## API

| Method | Path | |
|---|---|---|
| `GET` | `/healthz` | `ok` |
| `GET` | `/api/projects` | Project summaries |
| `GET` | `/api/projects/:id` | A project; its revision in `X-Revision` |
| `GET` | `/api/projects/:id/summary` | Its summary (cheap; editors poll it) |
| `PUT` | `/api/projects/:id` | Save; `X-Base-Revision` names the revision it started from, 409 if stale |
| `POST` | `/api/assets` | Upload an image, video, audio or font file (streamed, 512 MB at most) |
| `GET` | `/api/assets/:id` | Download it (range requests work) |
| `POST` | `/api/renders` | Queue a render for the Mac: `{"project_id", "preset"}` |
| `GET` | `/api/renders` | Render statuses |
| `GET` | `/api/renders/:id/video` | The finished video (MP4, or a ProRes .mov) |

The server has no login of its own; put it behind a single sign-on gate (at
example.com, Caddy asks an OAuth2 proxy first).

## MCP

`anima2d serve --mcp-port 9216` adds an MCP server (streamable HTTP, `POST /mcp`) so agents
can draw: `list_projects`, `get_project`, `create_project`, `add_layer`, `get_drawing`,
`add_items`, `replace_items`, `add_drawing`, `set_layer` (blend, clipping and motion keys),
and `item_format`, the reference of the item and motion JSON with examples. Items go through the same parser as the editor's saves, so a mistake
comes back as the exact rejection; writes are revisioned, so an open editor shows them.
On the server it is `https://example.com/anima2d/mcp`, behind a sign-in gate.

## The render agent

the server has no GPU, so renders run on the Mac. The server writes a job into
`renders/queue` under the projects folder, which the VM and the Mac share; the agent draws
each frame with the editor's own renderer (on Skia), pipes it to ffmpeg's VideoToolbox
encoder, and leaves the video in `renders/done`.

```sh
cd render && npm install && npm run build && ./install.sh
```

installs it as a LaunchAgent (`anima2d.render-agent`, logging to
`~/Library/Logs/anima2d-render.log`). Re-run after changing the project format, so the agent
parses what the editor writes.

## Deploying

A push to `main` runs `.forgejo/workflows/deploy.yaml` on the host's Actions runner: it
checks and builds the flake and hands the store path to the deploy socket the server sets
up (`the host configuration`), which switches the profile and rolls back if
`/healthz` does not answer.

## Licence

Apache-2.0.
