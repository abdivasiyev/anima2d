// anima2D's service worker: the app opens even when the server is out of reach.
//
// Pages: network first, the cached copy when the network fails or a proxy answers for
// a server it cannot reach (a proxy's 5xx). Built assets: their names carry a content
// hash, so a cached one is always right. The API is never cached here: projects live in
// the editor's own device storage (local.ts), which knows about revisions.
const CACHE = "anima2d-shell-v2";
const SHELL = "/";

/** The page and everything it loads: scripts and styles are named in it. */
const cacheShell = async () => {
  const c = await caches.open(CACHE);
  const res = await fetch(SHELL, { credentials: "same-origin" });
  if (!res.ok || res.type !== "basic") return;
  const html = await res.clone().text();
  await c.put(SHELL, res);
  const assets = [...html.matchAll(/(?:src|href)="(\/[^"]+)"/g)].map((m) => m[1]);
  await c.addAll([...new Set([...assets, "/manifest.webmanifest", "/icon.svg", "/icon-180.png"])]);
};

self.addEventListener("install", (event) => {
  event.waitUntil(
    cacheShell()
      .catch(() => undefined)
      .then(() => self.skipWaiting()),
  );
});

self.addEventListener("activate", (event) => {
  event.waitUntil(
    caches
      .keys()
      .then((keys) => Promise.all(keys.filter((k) => k !== CACHE).map((k) => caches.delete(k))))
      .then(() => self.clients.claim()),
  );
});

const unreachable = (res) => res.status >= 502 && res.status !== 501;

self.addEventListener("fetch", (event) => {
  const req = event.request;
  const url = new URL(req.url);
  if (req.method !== "GET" || url.origin !== self.location.origin) return;
  if (url.pathname.startsWith("/api/") || url.pathname.startsWith("/oauth2/") || url.pathname === "/healthz") return;

  if (req.mode === "navigate") {
    event.respondWith(
      fetch(req)
        .then((res) => {
          if (unreachable(res)) throw new Error("unreachable");
          // Only a real page is worth keeping; the sign-in redirect is not.
          // A fresh page after a deploy names new assets: cache them for the next outage.
          if (res.ok && res.type === "basic") void cacheShell().catch(() => undefined);
          return res;
        })
        .catch(() => caches.match(SHELL).then((hit) => hit || Response.error())),
    );
    return;
  }

  event.respondWith(
    caches.match(req).then(
      (hit) =>
        hit ||
        fetch(req).then((res) => {
          if (res.ok && res.type === "basic") {
            const copy = res.clone();
            void caches.open(CACHE).then((c) => c.put(req, copy));
          }
          return res;
        }),
    ),
  );
});
