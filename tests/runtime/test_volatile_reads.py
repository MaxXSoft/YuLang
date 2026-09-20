#!/usr/bin/env python3
"""Check observable volatile accesses, including discarded expression values."""

import argparse
from pathlib import Path
import re
import subprocess
import tempfile


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--yuc", type=Path, required=True)
    parser.add_argument("--work-dir", type=Path, required=True)
    args = parser.parse_args()
    args.work_dir.mkdir(parents=True, exist_ok=True)
    source = Path(__file__).with_name("volatile_reads.yu")
    # Loads, stores. Merely taking an address or binding a reference must
    # preserve neither the synthetic lvalue load nor an extra MMIO read.
    expected = {
        "read_discard": (1, 0),
        "read_twice": (2, 0),
        "read_unused_sum": (1, 0),
        "read_value": (1, 0),
        "write_value": (0, 1),
        "add_value": (1, 1),
        "address_of": (0, 0),
        "pass_reference": (0, 0),
        "read_field": (1, 0),
        "write_field": (0, 1),
        "read_indirect": (2, 0),
    }
    with tempfile.TemporaryDirectory(prefix="volatile-", dir=args.work_dir) as tmp:
        for level in (0, 2):
            output = Path(tmp) / f"volatile-O{level}.ll"
            subprocess.run([str(args.yuc), str(source), "-O", str(level),
                            "-ot", "llvm", "-o", str(output)], check=True)
            ir = output.read_text()
            for name, counts in expected.items():
                body = re.search(r"^define\b[^\n]*@" + name +
                                 r"\([^\n]*\)[^{]*\{(.*?)^\}",
                                 ir, re.M | re.S)
                assert body, f"O{level}: missing function {name}"
                actual = tuple(len(re.findall(r"\b" + op + r" volatile\b",
                                               body.group(1)))
                               for op in ("load", "store"))
                assert actual == counts, (
                    f"O{level} {name}: volatile loads/stores {actual}, "
                    f"expected {counts}")
            print(f"O{level}: volatile accesses passed")


if __name__ == "__main__":
    main()
