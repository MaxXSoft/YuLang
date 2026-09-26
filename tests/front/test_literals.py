"""Check contextual literal typing, strict boundaries, and target widths."""
import argparse
from pathlib import Path
import subprocess
import tempfile


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--yuc', type=Path, required=True)
    parser.add_argument('--work-dir', type=Path, required=True)
    args = parser.parse_args()
    args.work_dir.mkdir(parents=True, exist_ok=True)
    accepted = {
        'init': 'var x: u32 = 1 + 2',
        'assign': 'def f() { var x: u32 = 0; x = 1 + 2; x += 3 }',
        'symmetric': 'def f(x: u64): u64 { (1 + x) + (x + (2 + 3)) }',
        'wide': 'def f(): u64 { 1 << 40 }',
        'minimum': 'def f(): i64 { -9223372036854775808 }',
        'narrow_minimum': 'def f(): i8 { -128 }',
        'maximum': 'def f(): u64 { 18446744073709551615 }',
        'null': 'def f(p: u8*): bool { null == p && p != null }',
        'pointer_offset': 'def f(p: u8*): u8* { p + 1 }',
        'return': 'def f(c: bool): u32 { if c { return 1 }; 2 }',
        'if_peer': 'def f(c: bool, p: u8*): u8* { let q = if c { null } else { p }; q }',
        'if_integer': 'def f(c: bool, x: u32): u32 { let n = if c { 1 + 2 } else { x }; n }',
        'when': 'def f(x: u32): u64 { when x { 1 { 2 } 3, 4 { 5 } else { 6 } } }',
        'enum': 'enum E: u8 { A = 1, B = 2 + 3 }',
        'aggregate': 'struct S { n: u32, p: u8* }\nvar s = [S] {1 + 2, null}\nvar a = [u16[2]] {1, 2}',
        'single': 'def f(n: usize): usize { n }\nlet n = f(1 + 2)',
        'external': 'extern declare f: (u32)\ndef g() { f(1) }',
        'declared': 'declare f: (u32)\ndef g() { f(1) }',
        'function_value': 'def g(f: (u32): u32): u32 { f(1) }',
        'overload_unique': 'def f(x: u32, y: bool) {}\ndef f(x: u64, y: u8*) {}\ndef g() { f(1, true) }',
        'overload_exact': 'def f(x: i32): i32 { 1 }\ndef f(x: u32): u32 { 2 }\nlet n: i32 = f(1)',
        'overload_null': 'def f(x: u8*, y: bool) {}\ndef f(x: u32*, y: i32) {}\ndef g() { f(null, true) }',
        'operator': 'struct S {}\ndef +(s: S, x: u32): S { s }\ndef g(s: S): S { s + 1 }',
        'cast_boundary': 'def f(): u32 { 0xffffffff as u32 }\ndef g(): u8 { 256 as u8 }',
        'reference_assign': 'def f(x: u32 var&) { x = 1 }',
        'call_negative': 'def f(x: i8): i8 { x }\nlet n = f(-128)',
        'call_block': 'def f(x: u64): u64 { x }\nlet n = f({ let local = 1; 1 << 40 })',
        'three_overloads': 'def f(x: u8) {}\ndef f(x: u16) {}\ndef f(x: u64) {}\ndef g() { f(65536) }',
        'function_field': 'struct S { f: (u32): u32 }\ndef g(s: S&): u32 { (s.f)(1) }',
        'when_peer': 'def f(c: i32, x: u64): u64 { let n = when c { 0 { 1 } 1 { 2 } else { x } }; n }',
        'pointer_overload': 'struct S {}\ndef +(s: S, n: u32): S { s }\ndef g(p: u8*): u8* { p + 1 }',
        'block_scope': 'def f(): u64 { { let n = 1; n as u64 + 2 } }',
    }
    rejected = {
        'variable': 'def f(x: i32): u32 { x }',
        'constant': 'let x = 1\nlet y: u32 = x',
        'inline_constant': 'inline let x = 1\nlet y: u32 = x',
        'explicit_cast': 'let x: u32 = 1 as i32',
        'mixed_variables': 'def f(x: u32, y: i32): u32 { x + y }',
        'unsigned_negative': 'let x: u32 = -1',
        'unsigned_overflow': 'let x: u8 = 256',
        'signed_overflow': 'let x: i8 = 128',
        'signed_underflow': 'let x: i8 = -129',
        'sibling_overflow': 'def f(x: u8): u8 { x + 256 }',
        'call_overflow': 'def f(x: u8) {}\ndef g() { f(256) }',
        'aggregate_overflow': 'var x = [u8[1]] {256}',
        'bool': 'let x: bool = 1',
        'float': 'let x: f64 = 1',
        'character': "let x: u32 = 'a'",
        'integer_pointer': 'let x: u8* = 0',
        'null_integer': 'let x: usize = null',
        'untyped_null': 'let x = null',
        'reference': 'let x: u32& = 1',
        'reference_arg': 'def f(x: u32&) {}\ndef g() { f(1) }',
        'ambiguous': 'def f(x: u32) {}\ndef f(x: u64) {}\ndef g() { f(1) }',
        'ambiguous_null': 'def f(x: u8*) {}\ndef f(x: u32*) {}\ndef g() { f(null) }',
        'const_pointer': 'def f(p: u8 var*) {}\ndef g(p: u8*) { f(p) }',
        'enum_conversion': 'enum E { A }\nlet x: E = 0',
    }
    with tempfile.TemporaryDirectory(dir=args.work_dir, prefix='literals-') as directory:
        work = Path(directory)
        for valid, cases in ((True, accepted), (False, rejected)):
            for name, source in cases.items():
                path = work / f'{name}.yu'
                path.write_text(source + '\n')
                result = subprocess.run([str(args.yuc), str(path), '-ot', 'llvm',
                                         '-o', str(work / f'{name}.ll')],
                                        capture_output=True, text=True)
                assert result.returncode == (0 if valid else 1), (name, result.returncode, result.stderr)
                if name.startswith('ambiguous'):
                    assert 'ambiguous' in result.stderr, (name, result.stderr)
        for target, valid in (('riscv32-unknown-elf', False), ('x86_64-unknown-linux-gnu', True)):
            path = work / 'width.yu'
            path.write_text('var n: usize = 4294967296\n')
            result = subprocess.run([str(args.yuc), str(path), '-tt', target,
                                     '-ot', 'llvm', '-o', str(work / 'width.ll')],
                                    capture_output=True, text=True)
            assert result.returncode == (0 if valid else 1), (target, result.stderr)
    print(f'PASS: {len(accepted)} accepted, {len(rejected)} rejected, 2 target widths')


if __name__ == '__main__':
    main()
