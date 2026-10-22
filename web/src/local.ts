/**
 * The device's own copy of every project, in IndexedDB.
 *
 * Every edit lands here first, so nothing is lost when the server is out of reach (it
 * has no power, the network is down). What is stored is the wire JSON, and it is
 * parsed on the way out like anything else read from outside the program.
 */
import { andThen, err, ok, type Result } from "./domain/result";
import { parseProject, parseRevision, unparseProject, type Project, type ProjectId, type Revision } from "./domain/project";
import { object } from "./domain/wire";

const DB = "anima2d";
const STORE = "projects";

export interface LocalCopy {
  readonly project: Project;
  /** The server revision this copy was based on; 0 for a project the server has never seen. */
  readonly base: Revision;
  /** Whether this copy has changes the server does not have yet. */
  readonly dirty: boolean;
}

const open = (): Promise<IDBDatabase> =>
  new Promise((resolve, reject) => {
    const req = indexedDB.open(DB, 1);
    req.onupgradeneeded = () => {
      req.result.createObjectStore(STORE);
    };
    req.onsuccess = () => {
      resolve(req.result);
    };
    req.onerror = () => {
      reject(new Error("the device storage could not be opened"));
    };
  });

let db: Promise<IDBDatabase> | undefined;
const database = (): Promise<IDBDatabase> => (db ??= open());

const run = async <T>(mode: IDBTransactionMode, body: (s: IDBObjectStore) => IDBRequest<T>): Promise<T> => {
  const d = await database();
  return new Promise((resolve, reject) => {
    const req = body(d.transaction(STORE, mode).objectStore(STORE));
    req.onsuccess = () => {
      resolve(req.result);
    };
    req.onerror = () => {
      reject(new Error("the device storage refused"));
    };
  });
};

const parseCopy = (raw: unknown): Result<LocalCopy> =>
  andThen(object(raw), (o) =>
    andThen(parseProject(o.project), (project) =>
      andThen(parseRevision(o.base), (base) => (typeof o.dirty === "boolean" ? ok({ project, base, dirty: o.dirty }) : err("expected a dirty flag"))),
    ),
  );

export const putLocal = async (copy: LocalCopy): Promise<Result<null>> => {
  try {
    await run("readwrite", (s) => s.put({ project: unparseProject(copy.project), base: copy.base, dirty: copy.dirty }, copy.project.id));
    return ok(null);
  } catch {
    return err("the device storage refused the save");
  }
};

export const getLocal = async (id: ProjectId): Promise<Result<LocalCopy | undefined>> => {
  try {
    const raw: unknown = await run("readonly", (s) => s.get(id));
    if (raw === undefined) return ok(undefined);
    return parseCopy(raw);
  } catch {
    return err("the device storage could not be read");
  }
};

/** Forget this device's copy of a project. */
export const deleteLocal = async (id: ProjectId): Promise<Result<null>> => {
  try {
    await run("readwrite", (s) => s.delete(id));
    return ok(null);
  } catch {
    return err("the device storage refused the delete");
  }
};

export const allLocal = async (): Promise<LocalCopy[]> => {
  try {
    const raws: unknown = await run("readonly", (s) => s.getAll());
    if (!Array.isArray(raws)) return [];
    return raws.flatMap((r: unknown) => {
      const c = parseCopy(r);
      return c.ok ? [c.value] : [];
    });
  } catch {
    return [];
  }
};
