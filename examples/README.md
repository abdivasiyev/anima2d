# Examples

Project files to import into anima2D (**Import…** on the project list), with previews in
[`media/`](media).

| File | Shows |
|---|---|
| `bouncing-ball.anima2d.json` | Frame-by-frame animation on twos: squash and stretch, marks, a shadow under the line. |
| `sunset-walk.anima2d.json` | Parallax depth, a light layer, a walk cycle with drop shadow and rim light, a camera move with depth of field and tint. |
| `title-card.anima2d.json` | Text animated with layer keyframes, a typewriter subtitle, a moving light. |

They are written by [`build.ts`](build.ts) from plain data, checked by the editor's own
parser, and rendered with the editor's own renderer; run `just examples` after changing
it or the renderer. Text uses the browser's own fonts, so the files need no uploads.
