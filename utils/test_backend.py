#!/usr/bin/env python3
"""Exercise in-process LLVM emission, linking and execution at every -O level."""

import argparse
import os
from pathlib import Path
import subprocess
import sys
import tempfile


def run(*args):
    return subprocess.run([str(arg) for arg in args], check=True,
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--yuc", type=Path, required=True)
    parser.add_argument("--llvm-bin", type=Path, required=True)
    parser.add_argument("--cc", required=True)
    args = parser.parse_args()
    # Match the existing Linux build: LLVM's default relocation model is static.
    link_flags = ["-no-pie"] if sys.platform.startswith("linux") else []
    root = Path(__file__).resolve().parent.parent
    fixture = root / "tests/backend/zeros.yu"

    scratch = root / "debug"
    scratch.mkdir(exist_ok=True)
    with tempfile.TemporaryDirectory(prefix="backend-", dir=scratch) as temporary:
        work = Path(temporary)
        # Keep the compiler driver's own temporary files in the repository too.
        os.environ["TMPDIR"] = str(work)
        helper = work / "check_zeros.o"
        run(args.cc, "-c", root / "tests/backend/check_zeros.c", "-o", helper)
        for level in range(4):
            compiler = (args.yuc, "-O", level, fixture)
            obj = work / "zeros.o"
            # Default output is an object, with no external llc involved.
            run(*compiler, "-o", obj)
            exe = work / "zeros"
            run(args.cc, *link_flags, obj, helper, "-o", exe)
            run(exe)

            assembly = work / "zeros.s"
            run(*compiler, "-ot", "asm", "-o", assembly)
            run(args.cc, *link_flags, assembly, helper, "-o", exe)
            run(exe)

            ir = work / "zeros.ll"
            run(*compiler, "-ot", "llvm", "-o", ir)
            stdout = run(*compiler, "-ot", "llvm").stdout
            assert ir.read_bytes() == stdout, "IR file differs from stdout"
            run(args.llvm_bin / "opt", "-passes=verify", "-disable-output", ir)
            run(args.llvm_bin / "llc", "-filetype=obj", ir, "-o", obj)
            run(args.cc, *link_flags, obj, helper, "-o", exe)
            run(exe)
            print(f"O{level}: object, assembly and IR round trip passed")

        # All text outputs must reach disk even when they fit in the C++
        # stream buffer. Also cover errors before entering the LLVM backend.
        for kind in ("ast", "yuir"):
            output = work / f"zeros.{kind}"
            run(args.yuc, fixture, "-ot", kind, "-o", output)
            assert output.read_bytes() == run(
                args.yuc, fixture, "-ot", kind).stdout
        for kind in ("llvm", "asm", "obj"):
            result = subprocess.run(
                [str(args.yuc), str(fixture), "-ot", kind, "-o",
                 str(work / "missing" / "output")], capture_output=True)
            assert result.returncode != 0, "invalid output path was accepted"
        missing_obj = work / "missing.o"
        result = subprocess.run(
            [str(args.yuc), str(work / "missing.yu"), "-o", str(missing_obj)],
            capture_output=True)
        assert result.returncode != 0 and not missing_obj.exists()
        print("Text output and error handling passed")


if __name__ == "__main__":
    try:
        main()
    except subprocess.CalledProcessError as error:
        print(error.stderr.decode(errors="replace"))
        raise
