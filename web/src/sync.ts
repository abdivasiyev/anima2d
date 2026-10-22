/**
 * Keeping the device copy and the server in step.
 *
 * Every edit is written to the device first (local.ts) and pushed to the server shortly
 * after. With the server out of reach, edits keep landing on the device and go up once it
 * answers again. A push names the revision it started from; if someone else saved in the
 * meantime (another device, an agent over MCP), the server refuses it and this version is
 * kept as a copy rather than thrown away or forced over theirs.
 */
import { listProjects, loadProject, loadSummary, saveProject, waitForChanges } from "./api";
import { allLocal, getLocal, putLocal, type LocalCopy } from "./local";
import { err, ok, type Result } from "./domain/result";
import {
  localSummary,
  makeProject,
  parseProjectId,
  parseProjectName,
  parseRevision,
  type Project,
  type ProjectId,
  type ProjectSummary,
  type Revision,
} from "./domain/project";

export type SyncState = "saved" | "device" | "pending" | "saving" | "failed";

const ZERO: Revision = (() => {
  const r = parseRevision(0);
  if (!r.ok) throw new Error("revision 0");
  return r.value;
})();

const PUSH_DELAY_MS = 1200;
/** A look for news even when waiting on the server fails (an old server, a flaky link). */
const POLL_MS = 30000;

/** Your version, as a new project beside theirs. */
const asCopy = (p: Project): Result<Project> => {
  const id = parseProjectId(crypto.randomUUID());
  const name = parseProjectName(`${p.name.slice(0, 70)} (copy)`);
  if (!id.ok) return id;
  if (!name.ok) return name;
  return makeProject(id.value, name.value, p.canvas, p.fps, p.layers, p.camera, p.audio, p.palette, p.fonts);
};

/** Push a project the server has not got; used for copies and projects made offline. */
const pushNew = async (p: Project): Promise<void> => {
  await putLocal({ project: p, base: ZERO, dirty: true });
  const r = await saveProject(p, ZERO);
  if (r.kind === "ok") await putLocal({ project: p, base: r.value.revision, dirty: false });
};

export interface Opened {
  readonly project: Project;
  readonly base: Revision;
  /** Whether it has edits the server has not seen yet. */
  readonly dirty: boolean;
  /** Something the user should be told (opened offline, a copy was made). */
  readonly note?: string;
}

export const openProject = async (id: ProjectId): Promise<Result<Opened>> => {
  const [server, local] = await Promise.all([loadProject(id), getLocal(id)]);
  const copy: LocalCopy | undefined = local.ok ? local.value : undefined;
  if (server.kind === "ok") {
    const { project, revision } = server.value;
    if (copy?.dirty === true) {
      // Unsynced edits made on top of what the server still has: carry on with them.
      if (copy.base === revision) return ok({ project: copy.project, base: revision, dirty: true });
      // Made on top of something the server has since replaced: keep them as a copy.
      const kept = asCopy(copy.project);
      if (kept.ok) await pushNew(kept.value);
      await putLocal({ project, base: revision, dirty: false });
      return ok({ project, base: revision, dirty: false, note: kept.ok ? `Changed elsewhere; your edits were kept as “${kept.value.name}”` : "Changed elsewhere" });
    }
    await putLocal({ project, base: revision, dirty: false });
    return ok({ project, base: revision, dirty: false });
  }
  if (copy !== undefined) {
    const note = server.kind === "offline" ? "The server is out of reach; working on this device's copy" : undefined;
    return ok({ project: copy.project, base: copy.base, dirty: copy.dirty, ...(note === undefined ? {} : { note }) });
  }
  if (server.kind === "offline") return err("the server is out of reach and this project is not on this device yet");
  if (server.kind === "conflict") return err("the server reported a conflict");
  return err(server.error);
};

/** The server's projects plus any the server has not seen yet; the device's alone when offline. */
export const listAll = async (): Promise<Result<{ projects: ProjectSummary[]; offline: boolean }>> => {
  const [server, locals] = await Promise.all([listProjects(), allLocal()]);
  if (server.kind === "ok") {
    const known = new Set<string>(server.value.map((s) => s.id));
    const extra = locals.filter((c) => !known.has(c.project.id)).map((c) => localSummary(c.project));
    // Push what the server is missing (made offline) while the list is open.
    for (const c of locals) if (!known.has(c.project.id) && c.dirty) void pushNew(c.project);
    return ok({ projects: [...server.value, ...extra], offline: false });
  }
  if (server.kind === "offline") return ok({ projects: locals.map((c) => localSummary(c.project)).sort((a, b) => a.name.localeCompare(b.name)), offline: true });
  return err(server.kind === "error" ? server.error : "the server reported a conflict");
};

export const createProject = async (p: Project): Promise<void> => {
  await pushNew(p);
};

/** The sync for one open project. */
export class ProjectSync {
  private latest: Project;
  private base: Revision;
  private dirty: boolean;
  private timer = 0;
  private pushing: Promise<void> | undefined;
  private retry = 5000;
  private pollTimer = 0;
  private stopped = false;
  private readonly watching = new AbortController();

  constructor(
    opened: Opened,
    private readonly onState: (s: SyncState) => void,
    /** The server has a newer version (another device, an agent); show it. */
    private readonly onRemote: (p: Project, note: string) => void,
  ) {
    this.latest = opened.project;
    this.base = opened.base;
    this.dirty = opened.dirty;
    this.onState(this.dirty ? "pending" : "saved");
    if (this.dirty) this.schedule(0);
    this.pollTimer = window.setInterval(() => void this.poll(), POLL_MS);
    void this.watch();
  }

  /** Wait on the server for saves (long-polling) and look for a newer version after each. */
  private async watch(): Promise<void> {
    let seq: number | undefined;
    while (!this.isStopped()) {
      const r = await waitForChanges(seq, this.watching.signal);
      if (this.isStopped()) return;
      if (r.kind !== "ok") {
        // Offline or refused: the interval keeps looking; try waiting again in a while.
        seq = undefined;
        await new Promise((done) => setTimeout(done, POLL_MS));
        continue;
      }
      const news = seq !== undefined && r.value !== seq;
      seq = r.value;
      if (news) await this.poll();
    }
  }

  /** Back in front: catch up on anything saved while hidden. */
  resume(): void {
    void this.poll();
  }

  get hasUnsaved(): boolean {
    return this.dirty || this.pushing !== undefined;
  }

  // Read through methods: these change while a request is out.
  private isStopped(): boolean {
    return this.stopped;
  }

  private isDirty(): boolean {
    return this.dirty;
  }

  /** Read through a method: `edit` can run while a push awaits, which flow analysis cannot see. */
  private editedSince(snapshot: Project): boolean {
    return this.dirty || this.latest !== snapshot;
  }

  /** Record an edit: on the device now, on the server soon. */
  edit(p: Project): void {
    this.latest = p;
    this.dirty = true;
    void putLocal({ project: p, base: this.base, dirty: true });
    this.onState("pending");
    this.schedule(PUSH_DELAY_MS);
  }

  private schedule(ms: number): void {
    window.clearTimeout(this.timer);
    this.timer = window.setTimeout(() => void this.flush(), ms);
  }

  /** Push now (leaving the editor, the tab going to the background). */
  async flush(): Promise<void> {
    window.clearTimeout(this.timer);
    if (this.pushing !== undefined) await this.pushing;
    if (!this.dirty || this.stopped) return;
    const snapshot = this.latest;
    this.dirty = false;
    this.onState("saving");
    this.pushing = (async () => {
      const r = await saveProject(snapshot, this.base);
      this.pushing = undefined;
      switch (r.kind) {
        case "ok":
          this.base = r.value.revision;
          this.retry = 5000;
          // Edits made while the push was in flight are still waiting.
          await putLocal({ project: this.latest, base: this.base, dirty: this.editedSince(snapshot) });
          this.onState(this.editedSince(snapshot) ? "pending" : "saved");
          if (this.editedSince(snapshot)) this.schedule(PUSH_DELAY_MS);
          break;
        case "offline":
          // On the device already; try the server again, less often the longer it is gone.
          this.dirty = true;
          this.onState("device");
          this.schedule(this.retry);
          this.retry = Math.min(60000, this.retry * 2);
          break;
        case "conflict":
          await this.conflict(r.revision);
          break;
        case "error":
          this.dirty = true;
          this.onState("failed");
          this.schedule(this.retry);
          this.retry = Math.min(60000, this.retry * 2);
          break;
      }
    })();
    await this.pushing;
  }

  /** Someone saved first: keep this version as a copy and show theirs. */
  private async conflict(now: Revision): Promise<void> {
    const kept = asCopy(this.latest);
    if (kept.ok) await pushNew(kept.value);
    const theirs = await loadProject(this.latest.id);
    if (theirs.kind === "ok") {
      this.latest = theirs.value.project;
      this.base = theirs.value.revision;
      this.dirty = false;
      await putLocal({ project: this.latest, base: this.base, dirty: false });
      this.onState("saved");
      this.onRemote(this.latest, kept.ok ? `Changed elsewhere; your version was kept as “${kept.value.name}”` : "Changed elsewhere");
    } else {
      this.base = now;
      this.dirty = true;
      this.onState("failed");
      this.schedule(this.retry);
    }
  }

  /** Quietly look for a newer version while nothing here is waiting to go up. */
  private async poll(): Promise<void> {
    if (this.dirty || this.pushing !== undefined || this.stopped || document.visibilityState !== "visible") return;
    const s = await loadSummary(this.latest.id);
    // An edit may have arrived while the request was out; it wins.
    if (s.kind !== "ok" || s.value.revision <= this.base || this.isDirty()) return;
    const fresh = await loadProject(this.latest.id);
    if (fresh.kind !== "ok" || this.isDirty()) return;
    this.latest = fresh.value.project;
    this.base = fresh.value.revision;
    await putLocal({ project: this.latest, base: this.base, dirty: false });
    this.onRemote(this.latest, "Updated from another device or an agent");
  }

  stop(): void {
    this.stopped = true;
    this.watching.abort();
    window.clearInterval(this.pollTimer);
  }
}
