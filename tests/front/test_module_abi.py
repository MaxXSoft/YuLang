#!/usr/bin/env python3
"""Check module ownership, inline dependencies and anonymous object linkage."""

import argparse
from pathlib import Path
import re
import subprocess
import sys
import tempfile


def run(*command):
    result = subprocess.run([str(arg) for arg in command], capture_output=True,
                            text=True, timeout=60)
    if result.returncode:
        raise RuntimeError(f"{command!r} exited {result.returncode}\n"
                           f"{result.stdout}{result.stderr}")
    return result.stdout


def function_line(ir, name):
    matches = [line for line in ir.splitlines()
               if line.startswith("define ") and
               re.search(r'@"?_YF[^\s(]*' + name + r"[^\s(]*\(", line)]
    assert len(matches) == 1, (name, matches)
    return matches[0]


def global_line(ir, name):
    matches = [line for line in ir.splitlines()
               if line.startswith("@") and name in line.split(" = ")[0]]
    assert len(matches) == 1, (name, matches)
    return matches[0]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--yuc", type=Path, required=True)
    parser.add_argument("--cc", required=True)
    parser.add_argument("--work-dir", type=Path, required=True)
    parser.add_argument("--llvm-bin", type=Path)
    parser.add_argument("--sdk", default="")
    args = parser.parse_args()
    args.work_dir.mkdir(parents=True, exist_ok=True)
    cc = [args.cc]
    if args.sdk:
        sdk = args.sdk if Path(args.sdk).is_dir() else run(
            "xcrun", "--sdk", args.sdk, "--show-sdk-path").strip()
        cc += ["-isysroot", sdk]
    link_flags = ["-no-pie"] if sys.platform.startswith("linux") else []

    with tempfile.TemporaryDirectory(prefix="module-abi-",
                                     dir=args.work_dir.resolve()) as temporary:
        work = Path(temporary)
        library = work / "library"
        library.mkdir()
        (library / "sample.lib.yu").write_text("")
        owner = library / "api.yu"
        owner.write_text("""extern declare recordInit: (): i32
var initialSeed: i32 = recordInit()
var privateZero: i32

def hiddenLeaf(n: i32): i32 { n + 1 }
def nextImpl(): i32 { hiddenLeaf(initialSeed) }
inline def privateBump(): i32 {
  privateZero += 1
  privateZero
}
public inline def bump(): i32 { privateBump() + nextImpl() }
public inline def address(): usize { &privateZero as usize }
public inline def helperAddress(): usize { nextImpl as usize }
public def readZero(): i32 { privateZero }
public def forward(): i32 { later() }
def later(): i32 { 7 }
""")
        consumers = []
        for name, value in (("left", 1), ("right", 2)):
            source = work / f"{name}.yu"
            source.write_text(f"""import sample.api as api
public def collision(): i32 {{ {value} }}
extern def {name}_bump(): i32 {{ api.bump() }}
extern def {name}_address(): usize {{ api.address() }}
extern def {name}_helper(): usize {{ api.helperAddress() }}
extern def {name}_local(): i32 {{ collision() }}
extern def {name}_read(): i32 {{ api.readZero() }}
extern def {name}_forward(): i32 {{ api.forward() }}
""")
            consumers.append(source)

        helper = work / "check.c"
        helper.write_text("""#include <stdint.h>
static int initialized;
int32_t recordInit(void) { ++initialized; return 40; }
extern int32_t left_bump(void), right_bump(void);
extern int32_t left_local(void), right_local(void);
extern int32_t left_read(void), right_read(void);
extern int32_t left_forward(void), right_forward(void);
extern uintptr_t left_address(void), right_address(void);
extern uintptr_t left_helper(void), right_helper(void);
int main(void) {
  if (initialized != 1 || left_read() != 0 || right_read() != 0) return 1;
  if (left_address() != right_address()) return 2;
  if (left_helper() != right_helper()) return 3;
  if (left_bump() != 42 || right_bump() != 43) return 4;
  if (left_read() != 2 || right_read() != 2 || initialized != 1) return 5;
  if (left_local() != 1 || right_local() != 2) return 6;
  if (left_forward() != 7 || right_forward() != 7) return 7;
  return 0;
}
""")
        helper_object = work / "check.o"
        run(*cc, "-std=c11", "-c", helper, "-o", helper_object)

        def compile_source(source, level, kind):
            output = work / f"{source.stem}-O{level}.{kind}"
            run(args.yuc, "-I", library, "-O", level, source,
                "-ot", "llvm" if kind == "ll" else "obj", "-o", output)
            return output

        owner_ir_path = compile_source(owner, 0, "ll")
        consumer_ir_path = compile_source(consumers[0], 0, "ll")
        owner_ir = owner_ir_path.read_text()
        consumer_ir = consumer_ir_path.read_text()
        assert "internal" in function_line(owner_ir, "hiddenLeaf")
        assert "internal" not in function_line(owner_ir, "nextImpl")
        assert "internal" in global_line(owner_ir, "initialSeed")
        assert "external" not in global_line(owner_ir, "privateZero")
        assert "internal" not in global_line(owner_ir, "privateZero")
        assert "external" in global_line(consumer_ir, "privateZero")
        assert "linkonce_odr" in function_line(consumer_ir, "privateBump")
        assert "linkonce_odr" in function_line(consumer_ir, "bump")
        assert "internal" in function_line(consumer_ir, "collision")
        assert "llvm.global_ctors" in owner_ir
        assert "llvm.global_ctors" not in consumer_ir
        if args.llvm_bin:
            for path in (owner_ir_path, consumer_ir_path):
                run(args.llvm_bin / "opt", "-passes=verify", "-disable-output",
                    path)

        for owner_level, consumer_level in ((0, 0), (2, 2), (2, 0), (0, 2)):
            objects = [compile_source(owner, owner_level, "o")]
            objects += [compile_source(source, consumer_level, "o")
                        for source in consumers]
            executable = work / "check"
            run(*cc, *link_flags, *objects, helper_object, "-o", executable)
            run(executable)
            print(f"owner O{owner_level}, consumers O{consumer_level}: passed")


if __name__ == "__main__":
    main()
