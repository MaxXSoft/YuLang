"""Compare language sizeof with LLVM's array stride on multiple data models."""

import argparse
from pathlib import Path
import re
import subprocess
import tempfile

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--yuc", type=Path, required=True)
parser.add_argument("--llvm-bin", type=Path, required=True)
parser.add_argument("--work-dir", type=Path, required=True)
args = parser.parse_args()
args.work_dir.mkdir(parents=True, exist_ok=True)
source = Path(__file__).with_name("struct_layout.yu")


def constant(ir, name):
    body = re.search(r"define\b[^@]*@" + name + r"\([^)]*\)[^{]*\{(.*?)\n\}",
                     ir, re.S)
    assert body, name
    result = re.search(r"ret i\d+ (\d+)", body[1])
    assert result, body[1]
    return int(result[1])


with tempfile.TemporaryDirectory(prefix="target-layout-",
                                 dir=args.work_dir) as temporary:
    for triple in ("x86_64-unknown-linux-gnu", "i386-unknown-linux-gnu",
                   "aarch64-unknown-linux-gnu", "riscv32-unknown-elf"):
        output = Path(temporary) / f"{triple}.ll"
        result = subprocess.run(
            [str(args.yuc), str(source), "-tt", triple, "-O", "2",
             "-ot", "llvm", "-o", str(output)], capture_output=True, text=True)
        if (result.returncode == 1 and
                "No available targets are compatible with triple" in result.stderr):
            print(f"{triple}: skipped (LLVM backend not built)")
            continue
        assert result.returncode == 0, result.stderr
        subprocess.run([str(args.llvm_bin / "opt"), "-passes=verify",
                        "-disable-output", str(output)], check=True)
        ir = output.read_text()
        for aggregate in ("padded", "nested", "arrays"):
            size = constant(ir, aggregate + "_size")
            stride = constant(ir, aggregate + "_stride")
            assert size == stride, (triple, aggregate, size, stride)
        assert constant(ir, "empty_size") == 0
        print(f"{triple}: sizeof matches LLVM layout")
