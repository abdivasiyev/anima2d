/**
 * Fullscreen, for working without the browser's tabs and toolbars in view. Uses the
 * standard API where it exists and Safari's prefixed one otherwise. Installed to the home
 * screen (manifest `display: fullscreen`) the app is already fullscreen and the button hides.
 */
const root = (): HTMLElement => document.documentElement;

export const fullscreenSupported = (): boolean =>
  typeof root().requestFullscreen === "function" || typeof root().webkitRequestFullscreen === "function";

export const runningAsApp = (): boolean =>
  window.matchMedia("(display-mode: fullscreen)").matches || window.matchMedia("(display-mode: standalone)").matches;

export const isFullscreen = (): boolean => (document.fullscreenElement ?? document.webkitFullscreenElement ?? null) !== null;

export const toggleFullscreen = async (): Promise<void> => {
  if (isFullscreen()) {
    if (typeof document.exitFullscreen === "function") await document.exitFullscreen();
    else document.webkitExitFullscreen?.();
    return;
  }
  const el = root();
  if (typeof el.requestFullscreen === "function") await el.requestFullscreen({ navigationUI: "hide" });
  else el.webkitRequestFullscreen?.();
};

/** Call `fn` whenever fullscreen is entered or left. */
export const onFullscreenChange = (fn: () => void, signal: AbortSignal): void => {
  document.addEventListener("fullscreenchange", fn, { signal });
  document.addEventListener("webkitfullscreenchange", fn, { signal });
};
