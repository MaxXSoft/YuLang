#!/usr/bin/env python3
"""Check local variable shadowing without relaxing other duplicate diagnostics."""

import argparse
from pathlib import Path
import subprocess
import tempfile


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--yuc", type=Path, required=True)
    parser.add_argument("--work-dir", type=Path, required=True)
    args = parser.parse_args()
    args.work_dir.mkdir(parents=True, exist_ok=True)
    cases = [
        ("local_let", "def f(): i32 { let x = 1; let x = x + 1; x }", None),
        ("type_change", "def f(): bool { let x = 1; let x = x == 1; x }", None),
        ("global_let", "let x = 1\nlet x = 2", "identifier has already been defined"),
        ("global_var", "var x = 1\nlet x = 2", "identifier has already been defined"),
        ("global_after_function", "def f() {}\nlet x = 1\nlet x = 2",
         "identifier has already been defined"),
        ("local_var", "def f() { var x = 1; var x = x + 1 }", None),
        ("var_after_let", "def f() { let x = 1; var x = x + 1 }", None),
        ("let_after_var", "def f() { var x = 1; let x = x + 1 }", None),
        ("global_var_var", "var x = 1\nvar x = 2", "identifier has already been defined"),
        ("global_let_var", "let x = 1\nvar x = 2", "identifier has already been defined"),
        ("parameter", "def f(x: i32, x: i32) {}", "argument has already been declared"),
        ("self_init", "def f() { let x = x + 1 }", "identifier has not been defined"),
        ("var_self_init", "def f() { var x = x + 1 }", "identifier has not been defined"),
        ("immutable", "def f() { var x = 1; let x = x + 1; x = 3 }",
         "invalid binary operation"),
        ("runtime_dimension", "def f(n: i32) { let n = 2; let n = f_input(); var a: i32[n] }",
         "invalid array length"),
        ("mutable_dimension", "def f() { let n = 2; var n = 3; var a: i32[n] }",
         "invalid array length"),
        ("callable_signature", "def target(n: i32): i32 { n }\n"
         "def alternate(n: bool): bool { n }\n"
         "def f(): i32 { let target = target; let target = alternate; target(1) }",
         "invalid function call"),
    ]
    with tempfile.TemporaryDirectory(prefix="shadowing-", dir=args.work_dir) as temporary:
        work = Path(temporary)
        for name, source, diagnostic in cases:
            path = work / f"{name}.yu"
            path.write_text("extern declare f_input: (): i32\n" + source + "\n")
            output = work / f"{name}.ll"
            result = subprocess.run([str(args.yuc), str(path), "-ot", "llvm", "-o",
                                     str(output)], capture_output=True, text=True, timeout=30)
            if diagnostic is None:
                assert result.returncode == 0, (name, result.stderr)
            else:
                assert result.returncode == 1, (name, result.returncode, result.stderr)
                assert diagnostic in result.stderr, (name, result.stderr)
                assert not output.exists() or not output.read_bytes(), (
                    name, "invalid source produced output")


if __name__ == "__main__":
    main()
