#!/usr/bin/env python3
"""Compile Yu regression fixtures and run a native C oracle at each level."""

import argparse
from pathlib import Path
import subprocess
import sys
import tempfile


def run(*command):
    result = subprocess.run([str(arg) for arg in command], capture_output=True,
                            text=True, timeout=60)
    if result.returncode:
        raise RuntimeError(f"{command!r} exited {result.returncode}\n"
                           f"{result.stdout}{result.stderr}")
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--yuc", type=Path, required=True)
    parser.add_argument("--cc", required=True)
    parser.add_argument("--source", type=Path, required=True)
    parser.add_argument("--helper", type=Path, required=True)
    parser.add_argument("--module", type=Path, action="append", default=[])
    parser.add_argument("--import-path", type=Path,
                        default=Path(__file__).resolve().parents[2] / "lib")
    parser.add_argument("--work-dir", type=Path, required=True)
    parser.add_argument("--sdk", default="")
    parser.add_argument("--levels", type=int, nargs="+", default=[0, 2],
                        choices=range(4))
    args = parser.parse_args()
    args.work_dir.mkdir(parents=True, exist_ok=True)
    cc = [args.cc]
    if args.sdk:
        sdk = args.sdk
        if not Path(sdk).is_dir():
            sdk = run("xcrun", "--sdk", sdk, "--show-sdk-path").stdout.strip()
        cc += ["-isysroot", sdk]
    flags = ["-no-pie"] if sys.platform.startswith("linux") else []
    with tempfile.TemporaryDirectory(prefix=f"{args.source.stem}-",
                                     dir=args.work_dir.resolve()) as temporary:
        work = Path(temporary)
        helper = work / "helper.o"
        run(*cc, "-std=c11", "-c", args.helper, "-o", helper)
        for level in args.levels:
            objects = []
            for index, source in enumerate([args.source, *args.module]):
                obj = work / f"{index}.o"
                run(args.yuc, "-I", args.import_path, "-O", level,
                    "-ot", "obj", source, "-o", obj)
                objects.append(obj)
            executable = work / "check"
            run(*cc, *flags, *objects, helper, "-lm", "-o", executable)
            result = run(executable)
            print(f"{args.source.stem} O{level}: passed")
            if result.stdout:
                print(result.stdout, end="")


if __name__ == "__main__":
    main()
