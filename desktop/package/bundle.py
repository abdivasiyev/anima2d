#!/usr/bin/env python3
"""Copies every library the given Mach-O files load from the Nix store into a .app's
Frameworks folder, and points each reference at its copy.

Two different libraries can share a file name (Apple's libiconv and GNU's are both
libiconv.2.dylib), so each copy is keyed by where it came from, and a second one of a
name gets its store hash in front. @rpath references are resolved against the original
file's run paths.

    bundle.py <Frameworks dir> <file>=<original path> ...
"""

import os
import shutil
import stat
import subprocess
import sys

STORE = "/nix/store/"


def run(*args):
    return subprocess.run(args, check=True, capture_output=True, text=True).stdout


def own_id(path):
    out = run("otool", "-D", path).splitlines()
    return out[1].strip() if len(out) > 1 else None


def references(path):
    """The libraries `path` loads, as written in it."""
    me = own_id(path)
    refs = []
    for line in run("otool", "-L", path).splitlines()[1:]:
        ref = line.strip().split(" (compatibility")[0]
        if ref and ref != me:
            refs.append(ref)
    return refs


def run_paths(path):
    out = run("otool", "-l", path).splitlines()
    paths = []
    for i, line in enumerate(out):
        if line.strip() == "cmd LC_RPATH":
            for follow in out[i + 1 : i + 4]:
                follow = follow.strip()
                if follow.startswith("path "):
                    paths.append(follow.split(" ", 1)[1].split(" (offset")[0])
    return paths


def resolve(ref, original):
    """Where a reference written in `original` points to, if into the store."""
    if ref.startswith("@rpath/"):
        rest = ref[len("@rpath/") :]
        for rp in run_paths(original):
            rp = rp.replace("@loader_path", os.path.dirname(original))
            candidate = os.path.join(rp, rest)
            if os.path.exists(candidate):
                return os.path.realpath(candidate)
        return None
    if ref.startswith("@loader_path/"):
        candidate = os.path.join(os.path.dirname(original), ref[len("@loader_path/") :])
        return os.path.realpath(candidate) if os.path.exists(candidate) else None
    if ref.startswith(STORE):
        return os.path.realpath(ref)
    return None


def main():
    frameworks = sys.argv[1]
    os.makedirs(frameworks, exist_ok=True)
    copies = {}  # real source path -> name in Frameworks
    taken = {}  # name -> real source path
    queue = []
    for arg in sys.argv[2:]:
        here, original = arg.split("=", 1)
        queue.append((here, original))

    def name_for(source):
        if source in copies:
            return copies[source], False
        base = os.path.basename(source)
        name = base
        if name in taken and taken[name] != source:
            name = source[len(STORE) :].split("-", 1)[0][:8] + "-" + base
        copies[source] = name
        taken[name] = source
        return name, True

    done = set()
    while queue:
        here, original = queue.pop()
        if here in done:
            continue
        done.add(here)
        changes = []
        for ref in references(here):
            source = resolve(ref, original)
            if source is None:
                if ref.startswith(STORE) or ref.startswith("@rpath/"):
                    sys.exit(f"{here}: cannot find {ref}")
                continue
            name, fresh = name_for(source)
            target = os.path.join(frameworks, name)
            if fresh:
                shutil.copyfile(source, target)
                os.chmod(target, stat.S_IRUSR | stat.S_IWUSR | stat.S_IRGRP | stat.S_IROTH | stat.S_IXUSR)
                run("install_name_tool", "-id", "@executable_path/../Frameworks/" + name, target)
                queue.append((target, source))
            changes += ["-change", ref, "@executable_path/../Frameworks/" + name]
        if changes:
            run("install_name_tool", *changes, here)
        # Run paths into the store are of no use where the app goes.
        for rp in run_paths(here):
            if rp.startswith(STORE):
                run("install_name_tool", "-delete_rpath", rp, here)
    print(f"{len(copies)} libraries copied in")


if __name__ == "__main__":
    main()
