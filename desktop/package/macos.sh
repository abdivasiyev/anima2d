#!/usr/bin/env bash
# Builds anima2D.app and a .dmg of it, needing nothing of Nix or Homebrew where it runs:
# the editor and ffmpeg from the flake's nixpkgs, every library they load copied in and
# pointed at, and GTK's own files (image loaders, settings schemas, icons, a font
# config). Signed ad hoc, as macOS needs to run it on Apple silicon.
#
# From the repository root, on macOS with Nix:
#   desktop/package/macos.sh [version]        -> dist/macos/anima2D-<version>-macos-<arch>.dmg
set -euo pipefail

version="${1:-0.1.0}"
out="${OUT:-dist/macos}"
arch="$(uname -m)"
here="$(cd "$(dirname "$0")" && pwd)"

pkg() { nix build --no-link --print-out-paths --inputs-from . "nixpkgs#$1" | tail -n 1; }

echo "Building the editor and gathering its runtime"
editor="$(nix build --no-link --print-out-paths .#desktop | tail -n 1)"
gtk="$(pkg gtk3)"
rsvg="$(pkg librsvg)"
pixbuf="$(pkg gdk-pixbuf)"
adwaita="$(pkg adwaita-icon-theme)"
hicolor="$(pkg hicolor-icon-theme)"
glibbin="$(pkg glib.dev)"
ffmpeg="$(pkg ffmpeg.bin)"

app="$out/anima2D.app"
rm -rf "$out"
mkdir -p "$app/Contents/MacOS" "$app/Contents/Frameworks" "$app/Contents/Resources"
res="$app/Contents/Resources"
loaders="$res/lib/gdk-pixbuf-2.0/2.10.0/loaders"
mkdir -p "$loaders" "$res/share/glib-2.0/schemas" "$res/share/icons" "$res/etc/fonts"

# The programs, writable so their library paths can be rewritten.
install -m 755 "$editor/bin/anima2d-desktop" "$app/Contents/MacOS/anima2d-desktop"
install -m 755 "$ffmpeg/bin/ffmpeg" "$app/Contents/MacOS/ffmpeg"

# Image loaders (SVG for GTK's theme, and the rest gdk-pixbuf brings), and their list
# with the paths left for the launcher to fill in.
for so in "$pixbuf"/lib/gdk-pixbuf-2.0/2.10.0/loaders/*.so "$rsvg"/lib/gdk-pixbuf-2.0/2.10.0/loaders/*.so; do
  install -m 755 "$so" "$loaders/"
done
sed -e '/^# LoaderDir/d' \
    -e "s|$pixbuf/lib/gdk-pixbuf-2.0/2.10.0/loaders/|@RES@/lib/gdk-pixbuf-2.0/2.10.0/loaders/|g" \
    -e "s|$rsvg/lib/gdk-pixbuf-2.0/2.10.0/loaders/|@RES@/lib/gdk-pixbuf-2.0/2.10.0/loaders/|g" \
    "$rsvg/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache" > "$loaders/../loaders.cache.in"
if grep -q /nix/store "$loaders/../loaders.cache.in"; then
  echo "the image loaders' list still names the Nix store" >&2
  exit 1
fi

# GTK's settings schemas (the file chooser needs them), compiled.
cp "$gtk"/share/gsettings-schemas/*/glib-2.0/schemas/*.gschema.xml "$res/share/glib-2.0/schemas/"
"$glibbin/bin/glib-compile-schemas" "$res/share/glib-2.0/schemas"

# Icons: Adwaita (GTK's own marks, the window buttons) and hicolor, its fallback.
cp -R "$adwaita/share/icons/Adwaita" "$res/share/icons/"
cp -R "$hicolor/share/icons/hicolor" "$res/share/icons/"
chmod -R u+w "$res/share"

# Fonts from where macOS keeps them.
cp "$here/fonts.conf" "$res/etc/fonts/fonts.conf"

# Every library the programs and loaders load, copied in and pointed at (bundle.py keeps
# two libraries of one file name apart).
echo "Copying the libraries in"
roots=("$app/Contents/MacOS/anima2d-desktop=$editor/bin/anima2d-desktop" "$app/Contents/MacOS/ffmpeg=$ffmpeg/bin/ffmpeg")
for so in "$loaders"/*.so; do
  name="$(basename "$so")"
  if [ -e "$pixbuf/lib/gdk-pixbuf-2.0/2.10.0/loaders/$name" ]; then from="$pixbuf"; else from="$rsvg"; fi
  roots+=("$so=$from/lib/gdk-pixbuf-2.0/2.10.0/loaders/$name")
  # A loader's own name still says where it was built.
  install_name_tool -id "@executable_path/../Resources/lib/gdk-pixbuf-2.0/2.10.0/loaders/$name" "$so" 2> /dev/null
done
python3 "$here/bundle.py" "$app/Contents/Frameworks" "${roots[@]}"

# Rewritten files lose their signatures: every one is signed again, ad hoc.
while IFS= read -r -d '' f; do
  if file "$f" | grep -q Mach-O; then codesign --force --sign - "$f" 2> /dev/null; fi
done < <(find "$app/Contents" -type f -print0)

# Nothing may still reach into the Nix store.
leaks=0
while IFS= read -r -d '' f; do
  if file "$f" | grep -q Mach-O && otool -L "$f" | tail -n +2 | grep -q /nix/store; then
    echo "still loads from the Nix store: $f" >&2
    otool -L "$f" | grep /nix/store >&2
    leaks=1
  fi
done < <(find "$app/Contents" -type f -print0)
[ "$leaks" = 0 ] || exit 1

# The launcher, the icon and the bundle's description.
install -m 755 "$here/launcher.sh" "$app/Contents/MacOS/anima2D"
sed -e "s/@VERSION@/$version/g" "$here/Info.plist" > "$app/Contents/Info.plist"
iconset="$(mktemp -d)/anima2D.iconset"
mkdir -p "$iconset"
"$rsvg/bin/rsvg-convert" -w 1024 -h 1024 web/public/icon.svg -o "$iconset/icon_512x512@2x.png"
for s in 16 32 128 256 512; do
  sips -z "$s" "$s" "$iconset/icon_512x512@2x.png" --out "$iconset/icon_${s}x${s}.png" > /dev/null
  sips -z $((s * 2)) $((s * 2)) "$iconset/icon_512x512@2x.png" --out "$iconset/icon_${s}x${s}@2x.png" > /dev/null
done
iconutil -c icns "$iconset" -o "$res/anima2D.icns"

# Signed ad hoc: Apple silicon runs nothing unsigned.
codesign --force --deep --sign - "$app" 2> /dev/null
codesign --verify --deep --strict "$app"

echo "Making the disk image"
stage="$(mktemp -d)"
cp -R "$app" "$stage/"
ln -s /Applications "$stage/Applications"
dmg="$out/anima2D-$version-macos-$arch.dmg"
hdiutil create -volname "anima2D $version" -srcfolder "$stage" -ov -format UDZO "$dmg" > /dev/null
rm -rf "$stage"
echo "$dmg"
