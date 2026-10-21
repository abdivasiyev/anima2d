#!/usr/bin/env bash
# Install (or update) the render agent on this Mac as a LaunchAgent.
#
#   cd render && npm install && npm run build && ./install.sh
#
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
nix build nixpkgs#ffmpeg.bin --out-link "$home/ffmpeg"

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
    <key>ANIMA2D_FFMPEG</key><string>$home/ffmpeg/bin/ffmpeg</string>
    <key>ANIMA2D_DATA</key><string>$HOME/anima2d/projects</string>
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
launchctl bootstrap "gui/$(id -u)" "$plist"
echo "installed $label; log: ~/Library/Logs/anima2d-render.log"
