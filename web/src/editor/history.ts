/**
 * Undo and redo over immutable values. Projects share every unchanged frame between
 * versions, so keeping a couple of hundred of them costs little.
 */
export class History<T> {
  private past: T[] = [];
  private future: T[] = [];

  constructor(
    private present: T,
    private readonly limit = 200,
  ) {}

  get current(): T {
    return this.present;
  }

  get canUndo(): boolean {
    return this.past.length > 0;
  }

  get canRedo(): boolean {
    return this.future.length > 0;
  }

  /** Make `next` current; whatever was undone can no longer be redone. */
  commit(next: T): void {
    this.past.push(this.present);
    if (this.past.length > this.limit) this.past.shift();
    this.present = next;
    this.future = [];
  }

  undo(): boolean {
    const prev = this.past.pop();
    if (prev === undefined) return false;
    this.future.push(this.present);
    this.present = prev;
    return true;
  }

  redo(): boolean {
    const next = this.future.pop();
    if (next === undefined) return false;
    this.past.push(this.present);
    this.present = next;
    return true;
  }
}
