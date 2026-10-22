# Changelog

## Unreleased

* Layer keyframes: the Transform tool (Q) moves, scales and turns a whole layer about its
  pivot and keys it at the playhead; keys on the timeline with easing and fades; onion
  skins follow the motion.
* Layer blend modes (multiply, screen, overlay, add, darken, lighten) and clipping to the
  layer below.
* Text (T) in a chosen font, size, style and alignment; font files added to the project;
  Typewriter types a text on. Pictures as items, and background pictures that export.
* Project colours, with recolour everywhere; title/action-safe frames and a thirds grid;
  shift and trace.
* Exports: PNG frames (ZIP), animated GIF, transparent backgrounds, ProRes 4444 with alpha
  on the Mac.
* The toolbar moves to either edge; the timeline resizes; the page never zooms on a
  double tap or pinch on the iPad; playback stays responsive on the iPad.
* MCP: set_layer (blend, clipping, motion keys); text and image items; preview (a PNG
  of a frame), undo and redo.
* Near-instant sync between devices (long-polling); delete projects (to the server's
  trash); clicking empty timeline space clears the selection.

## 0.1.0.0

* Pencil-first drawing: pressure, stabiliser, palm rejection, brushes (pen, pencil,
  marker, paint), scatter brushes, shapes, curves, fill bucket, erasers, selection,
  symmetry, eyedropper.
* Project format 3: layers (drawing, background, reference), cels with holds and marks,
  camera keys, audio tracks, rigs and poses; formats 1 and 2 upgrade on read.
* Timeline with scrubbing, loops, ping-pong, speed, onion skin options, light table and
  sequence tools; inbetweening, motion paths, camera, audio, cut-out rigging.
* Offline: device copy in IndexedDB, revisioned saves, a cached app shell.
* Export presets; in-browser export (WebCodecs) and a GPU render agent for the Mac.
* MCP server for agents.
