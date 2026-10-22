#!/usr/bin/env bash
# Install (or update) the render agent on this Mac as a LaunchAgent.
#
#   cd render && npm install && npm run build && ANIMA2D_DATA=/path/to/projects ./install.sh
#
# ANIMA2D_DATA is the projects folder the server writes render jobs into, shared with this
# Mac; it defaults to ~/anima2d/projects.
# It runs as you (no sudo), starts at login, restarts if it stops, and logs to
# ~/Library/Logs/anima2d-render.log. ffmpeg comes from nixpkgs and is kept as a GC root
# beside the agent so a garbage collection cannot remove it from under a render.
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
home="$HOME/Library/Application Support/anima2d-render"
label=anima2d.render-agent
plist="$HOME/Library/LaunchAgents/$label.plist"
node=$(command -v node)

mkdir -p "$home" "$HOME/Library/Logs"
cp "$here/dist/agent.mjs" "$home/agent.mjs"
rm -rf "$home/node_modules"
mkdir -p "$home/node_modules"
cp -R "$here/node_modules/@napi-rs" "$home/node_modules/"
# A multi-output package's link is named after its output: ffmpeg-bin.
rm -f "$home/ffmpeg" "$home/ffmpeg-bin"
nix build nixpkgs#ffmpeg.bin --out-link "$home/ffmpeg"
ffmpeg=$(ls -d "$home"/ffmpeg* | head -n 1)/bin/ffmpeg
"$ffmpeg" -hide_banner -encoders | grep -q h264_videotoolbox || { echo "this ffmpeg has no VideoToolbox encoder" >&2; exit 1; }

cat > "$plist" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>Label</key><string>$label</string>
  <key>ProgramArguments</key>
  <array>
    <string>$node</string>
    <string>$home/agent.mjs</string>
  </array>
  <key>EnvironmentVariables</key>
  <dict>
    <key>ANIMA2D_FFMPEG</key><string>$ffmpeg</string>
    <key>ANIMA2D_DATA</key><string>${ANIMA2D_DATA:-$HOME/anima2d/projects}</string>
  </dict>
  <key>WorkingDirectory</key><string>$home</string>
  <key>RunAtLoad</key><true/>
  <key>KeepAlive</key><true/>
  <key>ThrottleInterval</key><integer>10</integer>
  <key>StandardOutPath</key><string>$HOME/Library/Logs/anima2d-render.log</string>
  <key>StandardErrorPath</key><string>$HOME/Library/Logs/anima2d-render.log</string>
</dict>
</plist>
PLIST

launchctl bootout "gui/$(id -u)/$label" 2>/dev/null || true
# The old instance takes a moment to unload; bootstrapping over it fails with error 5.
for _ in 1 2 3 4 5 6 7 8 9 10; do
  launchctl print "gui/$(id -u)/$label" >/dev/null 2>&1 || break
  sleep 1
done
launchctl bootstrap "gui/$(id -u)" "$plist"
echo "installed $label; log: ~/Library/Logs/anima2d-render.log"
