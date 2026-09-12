#!/usr/bin/env python3
"""Run one example and check its exit status and expected standard output."""

import argparse
import difflib
from pathlib import Path
import re
import subprocess
import sys


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--executable", type=Path, required=True)
    parser.add_argument("--input", type=Path)
    expected = parser.add_mutually_exclusive_group(required=True)
    expected.add_argument("--expected", type=Path)
    expected.add_argument("--pattern", type=Path)
    args = parser.parse_args()

    stdin = args.input.read_bytes() if args.input else b""
    result = subprocess.run([str(args.executable)], input=stdin,
                            capture_output=True, timeout=60)
    if result.stderr:
        sys.stderr.buffer.write(result.stderr)
    if result.returncode != 0:
        print(f"{args.executable.name}: exit status {result.returncode}",
              file=sys.stderr)
        sys.stderr.buffer.write(result.stdout)
        return 1

    actual = result.stdout.decode("utf-8", errors="replace")
    if args.expected:
        reference = args.expected.read_bytes()
        if result.stdout != reference:
            diff = difflib.unified_diff(
                reference.decode("utf-8", errors="replace").splitlines(keepends=True),
                actual.splitlines(keepends=True),
                fromfile=str(args.expected), tofile="actual stdout")
            sys.stderr.writelines(diff)
            return 1
    elif re.fullmatch(args.pattern.read_text(encoding="utf-8"), actual) is None:
        print(f"stdout does not match {args.pattern}:\n{actual}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (OSError, subprocess.TimeoutExpired) as error:
        print(error, file=sys.stderr)
        sys.exit(1)
