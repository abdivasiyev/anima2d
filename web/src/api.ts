/**
 * The projects API. Every response is decoded to `unknown` and parsed; a reply the editor
 * cannot parse is an error value, never a half-trusted object.
 *
 * "Offline" is its own outcome, distinct from a refusal: the server being out of reach is
 * normal here (a home server loses power), and the editor carries on from the device copy.
 */
import { andThen, each, err, ok, type Result } from "./domain/result";
import {
  parseProject,
  parseProjectSummary,
  parseRevision,
  parseSummaries,
  unparseProject,
  type Project,
  type ProjectId,
  type ProjectSummary,
  type Revision,
} from "./domain/project";
import { array, object } from "./domain/wire";

export type Reply<T> = { readonly kind: "ok"; readonly value: T } | { readonly kind: "offline" } | { readonly kind: "conflict"; readonly revision: Revision } | { readonly kind: "error"; readonly error: string };

const TIMEOUT_MS = 8000;

/** What the server sent: its status, a header, and the body decoded to `unknown`. */
interface Raw {
  readonly status: number;
  readonly revision: string | null;
  readonly body: unknown;
}

const request = async (path: string, init?: RequestInit): Promise<Reply<Raw>> => {
  let res: Response;
  try {
    res = await fetch(path, { credentials: "same-origin", signal: AbortSignal.timeout(TIMEOUT_MS), ...init });
  } catch {
    return { kind: "offline" };
  }
  // A proxy in front answers for a server it cannot reach (502, 503, 504, 52x, 530).
  if (res.status >= 502 && res.status !== 501) return { kind: "offline" };
  // The sign-in gate answers an expired session with a redirect to its sign-in page.
  if (res.redirected || res.status === 401) return { kind: "error", error: "your session has expired; reload to sign in again" };
  let body: unknown = null;
  try {
    body = await res.json();
  } catch {
    body = null;
  }
  if (res.status === 409) {
    const now = andThen(object(body), (o) => parseRevision(o.revision));
    return now.ok ? { kind: "conflict", revision: now.value } : { kind: "error", error: "the server reported a conflict it did not explain" };
  }
  if (!res.ok) return { kind: "error", error: res.status === 404 ? "no such project" : `the server answered ${res.status}` };
  return { kind: "ok", value: { status: res.status, revision: res.headers.get("X-Revision"), body } };
};

const parsed = <T>(r: Reply<Raw>, parse: (raw: Raw) => Result<T>): Reply<T> => {
  if (r.kind !== "ok") return r;
  const v = parse(r.value);
  return v.ok ? { kind: "ok", value: v.value } : { kind: "error", error: v.error };
};

/**
 * Wait for news: the server answers when anything is saved after save number `after`
 * (undefined asks for the current number at once), or after about 20 seconds with nothing
 * new. Answers the latest number.
 */
export const waitForChanges = async (after: number | undefined, signal: AbortSignal): Promise<Reply<number>> =>
  parsed(
    await request(after === undefined ? "/api/changes" : `/api/changes?after=${after}`, { signal: AbortSignal.any([signal, AbortSignal.timeout(35000)]) }),
    (r) => andThen(object(r.body), (o) => (typeof o.seq === "number" && Number.isInteger(o.seq) && o.seq >= 0 ? ok(o.seq) : err("the server sent no change number"))),
  );

/** Delete a project on the server (its files go to the server's trash). */
export const deleteProject = async (id: ProjectId): Promise<Reply<null>> =>
  parsed(await request(`/api/projects/${encodeURIComponent(id)}`, { method: "DELETE" }), () => ok(null));

export const listProjects = async (): Promise<Reply<ProjectSummary[]>> => parsed(await request("/api/projects"), (r) => parseSummaries(r.body));

export const loadProject = async (id: ProjectId): Promise<Reply<{ project: Project; revision: Revision }>> =>
  parsed(await request(`/api/projects/${encodeURIComponent(id)}`), (r) =>
    andThen(parseProject(r.body), (project) =>
      andThen(r.revision === null ? err("the server sent no revision") : parseRevision(Number(r.revision)), (revision) => ok({ project, revision })),
    ),
  );

export const loadSummary = async (id: ProjectId): Promise<Reply<ProjectSummary>> =>
  parsed(await request(`/api/projects/${encodeURIComponent(id)}/summary`), (r) => parseProjectSummary(r.body));

/** Save, if the server is still at `base`; base 0 saves a project the server has not seen. */
export const saveProject = async (p: Project, base: Revision): Promise<Reply<ProjectSummary>> => {
  const headers: Record<string, string> = { "Content-Type": "application/json" };
  if (base > 0) headers["X-Base-Revision"] = String(base);
  return parsed(
    await request(`/api/projects/${encodeURIComponent(p.id)}`, { method: "PUT", headers, body: JSON.stringify(unparseProject(p)) }),
    (r) => parseProjectSummary(r.body),
  );
};

/** Upload a reference image or video, or an audio file; the server answers with its id. */
export const uploadAsset = async (file: Blob): Promise<Reply<string>> =>
  parsed(
    await request("/api/assets", { method: "POST", headers: { "Content-Type": file.type || "application/octet-stream" }, body: file }),
    (r) => andThen(object(r.body), (o) => (typeof o.id === "string" ? ok(o.id) : err("the server sent no asset id"))),
  );

export const assetUrl = (id: string): string => `/api/assets/${encodeURIComponent(id)}`;

export interface RenderStatus {
  readonly id: string;
  readonly state: "queued" | "rendering" | "done" | "failed";
  readonly progress: number;
  readonly project: string;
  readonly preset: string;
  readonly created: string;
  readonly message: string;
}

const STATES = ["queued", "rendering", "done", "failed"] as const;

const parseStatus = (raw: unknown): Result<RenderStatus> =>
  andThen(object(raw), (o) => {
    const state = STATES.find((s) => s === o.state);
    const text = (v: unknown): string => (typeof v === "string" ? v : "");
    if (typeof o.id !== "string" || state === undefined || typeof o.progress !== "number") return err("expected a render status");
    return ok({ id: o.id, state, progress: o.progress, project: text(o.project), preset: text(o.preset), created: text(o.created), message: text(o.message) });
  });

/** Queue a render on the Mac; the server snapshots the project as it is saved now. */
export const queueRender = async (projectId: string, preset: unknown): Promise<Reply<string>> =>
  parsed(
    await request("/api/renders", { method: "POST", headers: { "Content-Type": "application/json" }, body: JSON.stringify({ project_id: projectId, preset }) }),
    (r) => andThen(object(r.body), (o) => (typeof o.id === "string" ? ok(o.id) : err("the server sent no render id"))),
  );

export const listRenders = async (): Promise<Reply<RenderStatus[]>> =>
  parsed(await request("/api/renders"), (r) => andThen(array(r.body), (xs) => each(xs, parseStatus)));

export const renderUrl = (id: string): string => `/api/renders/${encodeURIComponent(id)}/video`;
