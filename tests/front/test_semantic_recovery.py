"""Report independent semantic errors without leaking analysis contexts."""
import argparse
from pathlib import Path
import subprocess
import tempfile


parser = argparse.ArgumentParser()
parser.add_argument('--yuc', type=Path, required=True)
parser.add_argument('--work-dir', type=Path, required=True)
args = parser.parse_args()
args.work_dir.mkdir(parents=True, exist_ok=True)


with tempfile.TemporaryDirectory(dir=args.work_dir,
                                 prefix='semantic-recovery-') as directory:
    work = Path(directory)
    (work / 'test.lib.yu').write_text('')

    def reject(name, source, diagnostics, options=()):
        path = work / f'{name}.yu'
        output = work / f'{name}.ll'
        path.write_text(source, encoding='utf-8')
        result = subprocess.run(
            [str(args.yuc), '-I', str(work), str(path), '-ot', 'llvm', '-o', str(output),
             *options], capture_output=True, text=True, timeout=10)
        assert result.returncode == 1, (name, result.returncode, result.stderr)
        for diagnostic in diagnostics:
            assert diagnostic in result.stderr, (name, diagnostic, result.stderr)
        assert not output.read_text(), (name, 'generated IR after an error')
        return result.stderr

    reject('blocks', '''
def first() {
  missing_first
  missing_second
}
def second() { missing_third }
''', ['id: missing_first', 'id: missing_second', 'id: missing_third'])

    # Never evaluate a failed element, whose AST type is still unset.
    reject('bindings', '''
def bindings() {
  let first = missing_initializer, second = missing_next_initializer
  let third = missing_statement
}
''', ['id: missing_initializer', 'id: missing_next_initializer',
      'id: missing_statement'])

    loops = reject('loops', '''
extern declare next: (i32): i32
extern declare last: (i32): bool
def loops() {
  while true { missing_while }
  break
  var cursor = 0
  for item in cursor { missing_for }
  continue
}
def after_loops() { break }
''', ['id: missing_while', 'id: missing_for',
      'using break/continue outside the loop'])
    assert loops.count('using break/continue outside the loop') == 3, loops

    reject('struct_cleanup', '''
struct Broken { bad: MissingFieldType }
var value: Broken
def after_struct() { missing_after_struct }
''', ['id: MissingFieldType', 'id: Broken, type has not been defined',
      'id: missing_after_struct'])

    reject('enum_cleanup', '''
var Occupied = 0
enum Occupied { A }
var value: Occupied
''', ['enumeration name has already been used by another symbol',
      'id: Occupied, type has not been defined'])

    reject('property_context', '''
public let missing_type = { let local = 1; local }
def after_property() { missing_after_property }
''', ["'public' definitions must specify type explicitly",
      'id: missing_after_property'])

    reject('return_context', '''
def value(): i32 { 0 }
let outside = { return 1; 1 }
def after_return() { missing_after_return }
''', ['using return outside a function', 'id: missing_after_return'])

    reject('warning_error', '''
def bounds() {
  var values: i32[1]
  values[5]
  missing_after_warning
}
''', ['subscript out of bounds', 'id: missing_after_warning'], ['-Werror'])

    (work / 'broken_module.yu').write_text('''
extern declare runtime_value: (): i32
public enum BadConstant { A = runtime_value() }
public type FirstAlias = MissingImportTypeOne
public type SecondAlias = MissingImportTypeTwo
''', encoding='utf-8')
    reject('imports', '''
import test.broken_module
def after_import() { missing_after_import }
''', ['enumeration value must be a constant integer',
      'id: MissingImportTypeOne', 'id: MissingImportTypeTwo',
      'id: missing_after_import'])
