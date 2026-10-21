/**
 * The projects API. Every response is decoded to `unknown` and parsed; a reply the editor
 * cannot parse is an error value, never a half-trusted object.
 */
import { err, ok, type Result } from "./domain/result";
import {
  parseProject,
  parseProjectSummary,
  parseSummaries,
  unparseProject,
  type Project,
  type ProjectId,
  type ProjectSummary,
} from "./domain/project";

const request = async (path: string, init?: RequestInit): Promise<Result<unknown>> => {
  let res: Response;
  try {
    res = await fetch(path, { credentials: "same-origin", ...init });
  } catch {
    return err("the server could not be reached");
  }
  // The sign-in gate answers an expired session with a redirect to the sign-in provider.
  if (res.redirected || res.status === 401) return err("your session has expired; reload to sign in again");
  if (!res.ok) return err(res.status === 404 ? "no such project" : `the server answered ${res.status}`);
  try {
    const body: unknown = await res.json();
    return ok(body);
  } catch {
    return err("the server's reply was not JSON");
  }
};

const parsed = async <T>(r: Promise<Result<unknown>>, parse: (raw: unknown) => Result<T>): Promise<Result<T>> => {
  const raw = await r;
  return raw.ok ? parse(raw.value) : raw;
};

export const listProjects = (): Promise<Result<ProjectSummary[]>> => parsed(request("/api/projects"), parseSummaries);

export const loadProject = (id: ProjectId): Promise<Result<Project>> =>
  parsed(request(`/api/projects/${encodeURIComponent(id)}`), parseProject);

export const saveProject = (p: Project): Promise<Result<ProjectSummary>> =>
  parsed(
    request(`/api/projects/${encodeURIComponent(p.id)}`, {
      method: "PUT",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(unparseProject(p)),
    }),
    parseProjectSummary,
  );
