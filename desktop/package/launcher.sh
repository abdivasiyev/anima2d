#!/bin/sh
# anima2D.app's entry: points GTK at the files inside the app, then runs the editor.
contents="$(cd "$(dirname "$0")/.." && pwd)"
res="$contents/Resources"

# The image loaders' list names the loaders by path, so it is written for where the app is.
cache_dir="${TMPDIR:-/tmp}/anima2d-$(id -u)"
mkdir -p "$cache_dir"
sed "s|@RES@|$res|g" "$res/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache.in" > "$cache_dir/loaders.cache"

export GDK_PIXBUF_MODULE_FILE="$cache_dir/loaders.cache"
export XDG_DATA_DIRS="$res/share"
export GSETTINGS_SCHEMA_DIR="$res/share/glib-2.0/schemas"
export FONTCONFIG_FILE="$res/etc/fonts/fonts.conf"
export ANIMA2D_FFMPEG="$contents/MacOS/ffmpeg"
export LANG="${LANG:-en_US.UTF-8}"

exec "$contents/MacOS/anima2d-desktop" "$@"
