#!/usr/bin/env python3
"""Check module lookup parity, error recovery, and private construction."""

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
    yuc = args.yuc.resolve()
    with tempfile.TemporaryDirectory(prefix='module-recovery-',
                                     dir=args.work_dir.resolve()) as temporary:
        work = Path(temporary)
        library = work / 'library'
        library.mkdir()
        (library / 'sample.lib.yu').write_text('')

        def module(name, source):
            (library / f'{name}.yu').write_text(source + '\n')

        def check(name, source, diagnostic=None):
            path = work / f'{name}.yu'
            output = work / f'{name}.ll'
            dependency = Path(str(output) + '.d')
            path.write_text(source + '\n')
            result = subprocess.run(
                [str(yuc), '-I', str(library), str(path), '-O', '0',
                 '-ot', 'llvm', '-MD', '-o', str(output)],
                capture_output=True, text=True, timeout=20)
            expected = 1 if diagnostic else 0
            assert result.returncode == expected, (
                name, result.returncode, result.stderr)
            if diagnostic:
                assert diagnostic in result.stderr, (name, result.stderr)
                assert 'internal compiler error' not in result.stderr, (
                    name, result.stderr)
                assert not output.exists() or not output.read_bytes(), (
                    name, 'emitted output after semantic failure')
                assert not dependency.exists(), (
                    name, 'emitted dependency output after semantic failure')
                return ''
            assert output.is_file() and output.stat().st_size, name
            assert dependency.is_file(), name
            return output.read_text()

        module('invalid_value', 'public let value: i32 = unknown')
        module('invalid_type', 'public type Broken = Unknown')
        module('invalid_function', 'public def f(x: Unknown): i32 { 7 }')
        for binding, diagnostic in (
                ('invalid_value.value', 'identifier has not been defined'),
                ('invalid_type.Broken', 'type has not been defined'),
                ('invalid_function.f', 'type has not been defined')):
            check('invalid_' + binding.replace('.', '_'),
                  'import sample.' + binding, diagnostic)
        check('invalid_wildcard', 'import sample.invalid_type.*',
              'type has not been defined')

        factory = '''public struct Box { public x: i32 }
public def Box(): Box { [] {7} }'''
        module('factory', factory)
        check('bare_type_and_function', factory + '''
extern def inspect(): i32 { let b: Box = Box(); b.x }
''')
        selected = check('selected_type_and_function', '''
import sample.factory.Box
extern def inspect(): i32 { let b: Box = Box(); b.x }
''')
        qualified = check('qualified_type_and_function', '''
import sample.factory as f
extern def inspect(): i32 { let b: f.Box = f.Box(); b.x }
''')

        def called_symbols(ir):
            return re.findall(r'\bcall\b[^\n@]*@("[^"]+"|[^\s(]+)\(', ir)

        # The two import spellings must target the same module-owned factory.
        assert called_symbols(selected) == called_symbols(qualified)
        assert len(called_symbols(selected)) == 1

        enumeration = '''public enum E { A = 7 }
public def E(): E { E.A }'''
        module('enum_factory', enumeration)
        for name, imports, enum_name in (
                ('bare_enum_and_function', enumeration, 'E'),
                ('selected_enum_and_function',
                 'import sample.enum_factory.E', 'E'),
                ('qualified_enum_and_function',
                 'import sample.enum_factory as f', 'f.E')):
            ir = check(name, imports + f'''
extern def inspect(): i32 {{ {enum_name}.A as i32 }}
extern def construct(): {enum_name} {{ {enum_name}() }}
''')
            # Qualified enum access must keep constant evaluation intact.
            assert 'store i32 7,' in ir, (name, ir)

        function = 'def f(): i32 { 7 }\n'
        module('functions', 'public ' + function)
        check('local_value_shadows_function', function + '''
extern def inspect(): i32 { let f = 2; f() }
''', 'non-function')
        check('parameter_shadows_function', function + '''
extern def inspect(f: i32): i32 { f() }
''', 'non-function')
        check('local_value_shadows_import', '''
import sample.functions.f
extern def inspect(): i32 { let f = 2; f() }
''', 'non-function')
        check('call_before_local_binding', function + '''
extern def inspect(): i32 { let value = f(); let f = 2; value }
''')
        check('receiver_name_matches_local', '''
struct Box { value: i32 }
def x(this: Box&): i32 { this.value }
extern def inspect(): i32 { let x = [Box] {7}; x.x() }
''')

        module('private', '''
public struct Box { value: i32 }
public struct Outer { public box: Box }
public def make(): Box { [] {7} }
public def makeZero(): Box { [] {} }
public def read(this: Box&): i32 { this.value }
''')
        # Uninitialized storage keeps its existing zero-value semantics.
        # Visibility restrictions apply to explicit aggregate initializers.
        check('private_zero_storage', '''
import sample.private as foreign
extern def inspect(): i32 { var value: foreign.Box; value.read() }
''')
        check('private_array_construction', '''
import sample.private as foreign
extern def inspect(): i32 {
  var values: foreign.Box[1] = [] {}
  values[0].read()
}
''', 'private fields')
        check('private_omitted_nested_field', '''
import sample.private as foreign
extern def inspect(): i32 {
  let value: foreign.Outer = [] {}
  value.box.read()
}
''', 'private fields')
        check('private_explicit_nested_value', '''
import sample.private as foreign
extern def inspect(): i32 {
  let value: foreign.Outer = [] {foreign.make()}
  value.box.read()
}
''')


if __name__ == '__main__':
    main()
