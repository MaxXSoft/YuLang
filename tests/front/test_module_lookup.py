"""Module-owned lookup and whole-module declaration resolution regressions."""
import argparse
from pathlib import Path
import subprocess
import tempfile

parser = argparse.ArgumentParser()
parser.add_argument('--yuc', type=Path, required=True)
parser.add_argument('--work-dir', type=Path, required=True)
args = parser.parse_args()
args.work_dir.mkdir(parents=True, exist_ok=True)

with tempfile.TemporaryDirectory(dir=args.work_dir, prefix='module-lookup-') as directory:
    root = Path(directory)
    (root / 'sample.lib.yu').write_text('')
    (root / 'box.yu').write_text('''
public struct Box { public value: i32 }
public def make(value: i32): Box { [Box] {value} }
public def read(this: Box&): i32 { this.value }
public def choose(this: Box&, value: u32): u32 { value }
public def next(this: Box var&): i32 { this.value += 1; this.value }
public def last(this: Box&): bool { this.value == 3 }
public def +(left: i32, right: Box&): i32 { left + right.value }
public def +(left: Box*, right: i32): i32 { (*left).value + right }
def hidden(this: Box&): i32 { this.value }
''')
    (root / 'private.yu').write_text('''
struct Storage { value: i32 }
public struct Box { storage: Storage }
public def make(): Box { [Box] {[Storage] {7}} }
public def read(this: Box&): i32 { this.storage.value }
''')
    (root / 'extension.yu').write_text('''
import sample.box
public def extension(this: box.Box&): i32 { 9 }
''')
    (root / 'transitive.yu').write_text('''
import sample.extension.*
public struct Value {}
''')
    (root / 'other.yu').write_text('''
import sample.box
public def read(this: box.Box&): i32 { 99 }
''')

    def check(name, source, error=None, target=()):
        path = root / 'main.yu'
        output = root / 'out.ll'
        path.write_text(source)
        result = subprocess.run([str(args.yuc), '-I', str(root), str(path),
                                 '-ot', 'llvm', '-o', str(output), *target],
                                capture_output=True, text=True, timeout=15)
        assert result.returncode == (1 if error else 0), (name, result.returncode, result.stderr)
        if error:
            assert error in result.stderr, (name, error, result.stderr)
            assert not output.read_bytes(), (name, 'emitted output after failure')
        else:
            assert output.read_bytes(), name

    check('owner_dot', 'import sample.box\ndef f(): i32 { var b = box.make(7); b.read() }')
    check('owner_alias_type', 'import sample.box as b\ntype Alias = b.Box\ndef f(x: Alias&): i32 { x.read() }')
    check('owner_pointer', '''
import sample.box
public def unused() {}
def through(this: box.Box*): i32 { (*this).value }
def f(x: box.Box*): i32 { x.through() }
''')
    check('qualified_no_receiver', 'import sample.box\ndef f(): box.Box { box.make(7) }')
    check('bare_stays_lexical', 'import sample.box\ndef f(x: box.Box&): i32 { read(x) }', 'no matching function')
    check('owner_private_rejected', 'import sample.box\ndef f(x: box.Box&): i32 { x.hidden() }', 'no matching function')
    check('external_extension_private_field', '''
import sample.private
def reveal(this: private.Box&): i32 { this.storage.value }
''', 'structure field is private')
    check('visible_extension', '''
import sample.box
import sample.extension.extension
def f(x: box.Box&): i32 { x.extension() }
''')
    check('loaded_module_does_not_leak', '''
import sample.box
import sample.extension
def f(x: box.Box&): i32 { x.extension() }
''', 'no matching function')
    check('transitive_binding_does_not_leak', '''
import sample.box
import sample.transitive
def f(x: box.Box&): i32 { x.extension() }
''', 'no matching function')
    check('exact_extension_beats_literal_owner', '''
import sample.box
def choose(this: box.Box&, value: i32): bool { true }
def f(x: box.Box&): bool { x.choose(1) }
''')
    check('equal_receiver_candidates_ambiguous', '''
import sample.box
import sample.other.read
def f(x: box.Box&): i32 { x.read() }
''', 'ambiguous function call')
    check('same_definition_deduplicated', '''
import sample.box
import sample.box.read
def f(x: box.Box&): i32 { x.read() }
''')
    check('infix_owner', '''
import sample.box
public def sum(x: box.Box&): u32 { x choose 1 }
''')
    check('local_callable_infix', '''
import sample.box
def f(x: box.Box&, op: (box.Box&, i32): i32): i32 { x op 2 }
''')
    check('right_operand_owner', 'import sample.box\ndef f(x: box.Box&): i32 { 2 + x }')
    check('iterator_owner', '''
import sample.box
def f(): i32 { var sum = 0; var b = box.make(0); for n in b { sum += n }; sum }
''')
    check('function_field_parentheses', '''
struct Callable { call: (i32): i32 }
def f(x: Callable&): i32 { (x.call)(7) }
''')
    check('namespace_local_shadow', '''
import sample.box
struct Local { value: i32 }
def f(box: Local&): i32 { box.value }
''')
    check('forward_calls', 'def a(): i32 { b() }\ndef b(): i32 { 7 }')
    check('mutual_calls', '''
def even(x: i32): bool { if x == 0 { true } else { odd(x - 1) } }
def odd(x: i32): bool { if x == 0 { false } else { even(x - 1) } }
''')
    check('complete_overload_set', '''
def use(): u32 { select(1) }
def select(x: u32): u32 { x }
def select(x: u64): u64 { x }
''', 'ambiguous function call')
    check('forward_types_and_constants', '''
struct Box { data: Item[COUNT] }
type Item = i32
inline let COUNT = 4
def f(): usize { sizeof Box }
''')
    check('pointer_recursive_layout', 'struct A { b: B* }\nstruct B { a: A }\ndef f(): usize { sizeof B }')
    check('by_value_cycle', 'struct A { b: B }\nstruct B { a: A }', 'cyclic definition')
    check('alias_cycle', 'type A = B\ntype B = A', 'cyclic definition')
    check('constant_cycle', 'inline let A = B\ninline let B = A', 'cyclic definition')
    check('global_function_value', 'let function: (): i32 = later\ndef later(): i32 { 7 }\ndef f(): i32 { function() }')
    check('private_layout_in_public_struct', '''
import sample.private
def f(): i32 { var value = private.make(); value.read() }
''')

    check('pointer_operand_owner', 'import sample.box\ndef f(x: box.Box*): i32 { x + 2 }')
    check('no_implicit_receiver_dereference', 'import sample.box\ndef f(x: box.Box*): i32 { x.read() }', 'no matching function')
    check('same_width_exact_type', """
def select(x: usize): bool { true }
def select(x: u32): i32 { 0 }
def f(): bool { select(1 as usize) }
""", target=('-tt', 'riscv32-unknown-elf'))
    check('initializer_function_namespace', 'let value = value()\ndef value(): i32 { 7 }\ndef f(): i32 { value }')
