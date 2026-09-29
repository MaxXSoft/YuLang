"""Check omitted initializer types, diagnostics, and explicit/inferred IR parity."""
import argparse
from pathlib import Path
import re
import subprocess
import tempfile


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--yuc', type=Path, required=True)
    parser.add_argument('--work-dir', type=Path, required=True)
    args = parser.parse_args()
    args.work_dir.mkdir(parents=True, exist_ok=True)
    prefix = ('struct Cell { n: u32, p: u8* }\n'
              'struct Box { cell: Cell, numbers: u16[3] }\n'
              'struct Empty {}\n')
    # @Type@ denotes the same initializer with an explicit or omitted type.
    accepted = {
        'declaration': 'var s: Cell = @Cell@ {1, null}',
        'constant': 'let s: Cell = @Cell@ {1}',
        'zero': 'var s: Box = @Box@ {}',
        'nested': 'var s: Box = @Box@ {@Cell@ {2}, @u16[3]@ {3, 4}}',
        'explicit_outer': 'let s = [Box] {@Cell@ {2}, @u16[3]@ {3, 4}}',
        'array_return': 'def f(): u16[3] { @u16[3]@ {1, 2} }',
        'array': 'var s: Cell[2] = @Cell[2]@ {@Cell@ {5}, @Cell@ {6}}',
        'alias': 'type Item = Cell\nvar s: Item = @Item@ {1}',
        'empty': 'var s: Empty = @Empty@ {}',
        'assignment': 'def f() { var s: Cell; s = @Cell@ {7} }',
        'reference_assignment': 'def f(s: Cell var&) { s = @Cell@ {8} }',
        'member_assignment': 'def f(s: Box var&) { s.cell = @Cell@ {8} }',
        'tail': 'def f(): Cell { @Cell@ {9} }',
        'return': 'def f(c: bool): Cell { if c { return @Cell@ {10} }; @Cell@ {} }',
        'block': 'def f(): Cell { { let n: u32 = 11; @Cell@ {n} } }',
        'if': 'def f(c: bool): Cell { if c { @Cell@ {12} } else { @Cell@ {} } }',
        'when': 'def f(n: i32): Cell { when n { 0 { @Cell@ {13} } else { @Cell@ {} } } }',
        'call': 'def take(s: Cell): u32 { s.n }\nlet n = take(@Cell@ {14})',
        'array_call': 'def take(s: u16[3]): u16 { s[1] }\nlet n = take(@u16[3]@ {1, 2})',
        'two_arguments': 'def take(a: Cell, b: Box): u32 { a.n + b.cell.n }\nlet n = take(@Cell@ {1}, @Box@ {@Cell@ {2}})',
        'external': 'extern declare take: (Cell)\ndef f() { take(@Cell@ {15}) }',
        'declared': 'declare take: (Cell)\ndef f() { take(@Cell@ {16}) }',
        'function_value': 'def f(take: (Cell): u32): u32 { take(@Cell@ {17}) }',
        'function_field': 'struct F { take: (Cell): u32 }\ndef f(t: F&): u32 { (t.take)(@Cell@ {18}) }',
        'overload': 'def take(s: Cell, c: bool) {}\ndef take(s: Box, p: u8*) {}\ndef f() { take(@Cell@ {19}, true) }',
        'nonaggregate_overload': 'def take(n: i32) {}\ndef take(s: Cell) {}\ndef f() { take(@Cell@ {19}) }',
        'common_parameter': 'def take(s: Cell, n: i32) {}\ndef take(s: Cell, n: u32) {}\ndef f() { take(@Cell@ {20}, 1) }',
        'call_block': 'def take(s: Cell): u32 { s.n }\nlet n = take({ let local: u32 = 21; @Cell@ {local} })',
        'call_if': 'def take(s: Cell): u32 { s.n }\ndef f(c: bool): u32 { take(if c { @Cell@ {22} } else { @Cell@ {} }) }',
        'call_when': 'def take(s: Cell): u32 { s.n }\ndef f(n: i32): u32 { take(when n { 0 { @Cell@ {23} } else { @Cell@ {} } }) }',
        'dot_call': 'def take(s: Cell&, b: Box): u32 { b.cell.n }\ndef f(s: Cell&): u32 { s.take(@Box@ {@Cell@ {24}}) }',
    }
    rejected = {
        'no_context': ('let s = [] {1}', 'cannot infer initializer type'),
        'no_context_empty': ('let s = [] {}', 'cannot infer initializer type'),
        'discarded': ('def f() { [] {} }', 'initializer requires a structure or array type'),
        'too_many': ('let s: Cell = [] {1, null, 2}', 'initializer list length exceeded'),
        'nested_too_many': ('let s: Box = [] {[] {}, [] {1, 2, 3, 4}}', 'initializer list length exceeded'),
        'range': ('let s: Box = [] {[] {}, [] {65536}}', 'integer literal out of range'),
        'wrong_member': ('let s: Cell = [] {true}', 'invalid initializer list'),
        'ordinary_value': ('let n = 1\nlet s: Cell = [] {n}', 'invalid initializer list'),
        'pointer_const': ('def f(p: u8*) { var s: u8 var*[1] = [] {p} }', 'invalid initializer list'),
        'integer': ('let s: i32 = [] {}', 'initializer requires a structure or array type'),
        'pointer': ('let s: Cell* = [] {}', 'initializer requires a structure or array type'),
        'reference': ('let s: Cell& = [] {}', 'cannot initialize a reference'),
        'reference_call': ('def take(s: Cell&) {}\ndef f() { take([] {}) }', 'no matching function'),
        'cast_barrier': ('let s = [] {} as Cell', 'cannot infer initializer type'),
        'member_barrier': ('let n: u32 = ([] {1}).n', 'cannot infer initializer type'),
        'local_barrier': ('def f(): Cell { let local = [] {}; local }', 'cannot infer initializer type'),
        'ambiguous': ('def take(s: Cell) {}\ndef take(s: Box) {}\ndef f() { take([] {}) }', 'ambiguous initializer type'),
        'shape_is_not_type': ('def take(s: Cell) {}\ndef take(s: Box) {}\ndef f() { take([] {1, null}) }', 'ambiguous initializer type'),
        'same_shape': ('struct Other { n: u32, p: u8* }\ndef take(s: Cell) {}\ndef take(s: Other) {}\ndef f() { take([] {}) }', 'ambiguous initializer type'),
        'arity': ('def take(s: Cell, n: i32) {}\ndef f() { take([] {}) }', 'no matching function'),
        'call_member_type': ('def take(s: Cell) {}\ndef f() { take([] {true}) }', 'invalid initializer list'),
        'call_range': ('def take(s: Cell) {}\ndef f() { take([] {4294967296}) }', 'integer literal out of range'),
        'explicit_wrong_type': ('let s: Cell = [Box] {}', 'type mismatch when initializing'),
        'typed_scalar': ('let s = [i32] {}', 'initializer requires a structure or array type'),
    }
    with tempfile.TemporaryDirectory(dir=args.work_dir, prefix='initializers-') as directory:
        work = Path(directory)
        source = work / 'case.yu'

        def compile_source(body, *options):
            source.write_text(prefix + body + '\n')
            return subprocess.run([str(args.yuc), str(source), *options],
                                  capture_output=True, text=True, timeout=30)

        for name, body in accepted.items():
            explicit = re.sub(r'@([^@]+)@', r'[\1]', body)
            inferred = re.sub(r'@([^@]+)@', '[]', body)
            for target in ([], ['-tt', 'riscv32-unknown-elf']):
                for level in (0, 2):
                    options = ['-ot', 'llvm', '-O', str(level), *target]
                    before = compile_source(explicit, *options)
                    after = compile_source(inferred, *options)
                    assert before.returncode == 0, (name, before.stderr)
                    assert after.returncode == 0, (name, after.stderr)
                    assert before.stdout == after.stdout, (name, target, level, 'IR differs')
            result = compile_source(inferred, '-ot', 'ast')
            assert result.returncode == 0, (name, result.stderr)
        for name, (body, diagnostic) in rejected.items():
            result = compile_source(body, '-ot', 'llvm')
            assert result.returncode == 1, (name, result.returncode, result.stderr)
            assert diagnostic in result.stderr, (name, result.stderr)
    print(f'PASS: {len(accepted)} explicit/inferred pairs at O0/O2 on native/RV32; '
          f'{len(rejected)} rejected; AST dumps checked')


if __name__ == '__main__':
    main()
