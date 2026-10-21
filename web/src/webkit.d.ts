// Safari on iPad shipped the Fullscreen API prefixed for years; declare what we probe for.
interface Element {
  webkitRequestFullscreen?: () => void;
}

interface Document {
  webkitExitFullscreen?: () => void;
  readonly webkitFullscreenElement?: Element | null;
}
