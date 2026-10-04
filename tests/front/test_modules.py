#!/usr/bin/env python3
"""Check library identities, module imports, nominal types, and visibility."""

import argparse
from pathlib import Path
import re
import shutil
import subprocess
import tempfile


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--yuc', type=Path, required=True)
    parser.add_argument('--work-dir', type=Path, required=True)
    args = parser.parse_args()
    yuc = args.yuc.resolve()
    args.work_dir.mkdir(parents=True, exist_ok=True)
    count = 0
    with tempfile.TemporaryDirectory(prefix='modules-', dir=args.work_dir) as temporary:
        work = Path(temporary).resolve()
        library = work / 'library'
        library.mkdir()
        (library / 'test.lib.yu').write_text(' \t\r\n\v\f', encoding='utf-8')
        output_dir = work / 'outputs'
        output_dir.mkdir()
        inputs = work / 'inputs'
        inputs.mkdir()

        def write(relative, text):
            path = library / relative
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(text + '\n', encoding='utf-8')
            return path

        def compile_file(name, path, roots=None, diagnostic=None, cwd=None):
            nonlocal count
            output = output_dir / f'{name}.ll'
            command = [str(yuc), str(path), '-ot', 'llvm', '-O', '0',
                       '-MD', '-o', str(output)]
            for root in roots if roots is not None else [library]:
                command += ['-I', str(root)]
            result = subprocess.run(command, capture_output=True, text=True,
                                    timeout=30, cwd=cwd)
            if diagnostic is None:
                assert result.returncode == 0, (name, result.returncode, result.stderr)
                assert output.is_file() and output.stat().st_size, (name, result.stderr)
                assert Path(str(output) + '.d').is_file(), name
            else:
                assert result.returncode == 1, (name, result.returncode, result.stderr)
                for message in (diagnostic,) if isinstance(diagnostic, str) else diagnostic:
                    assert message in result.stderr, (name, message, result.stderr)
                assert not output.exists() or not output.read_bytes(), (
                    name, 'frontend error produced output')
                assert not Path(str(output) + '.d').exists(), (
                    name, 'frontend error produced dependency output')
            count += 1
            return output.read_text() if output.exists() else ''

        def check(name, source, diagnostic=None, roots=None):
            path = inputs / f'{name}.yu'
            path.write_text(source + '\n', encoding='utf-8')
            return compile_file(name, path, roots, diagnostic)

        write('a.yu', '''
public struct Item { public value: i32 }
public def make(x: i32): Item { [] {x} }
public def read(this: Item&): i32 { this.value }
public def same(x: i32): i32 { x }
public def same(x: bool): bool { x }
public var count: i32 = 1
struct Hidden { value: i32 }
def secret(): i32 { 8 }
''')
        write('b.yu', '''
public struct Item { public value: i32 }
public def make(x: i32): Item { [] {x} }
public var count: i32 = 2
''')
        check('namespace_isolation', '''
import test.a
import test.b
extern def inspect(): i32 {
  let x: a.Item = a.make(3)
  let y: b.Item = b.make(4)
  x.value + y.value + a.count + b.count
}
''')
        check('namespace_only', 'import test.a\ndef inspect(): i32 { count }',
              'identifier has not been defined')
        check('qualified_private', 'import test.a\ndef inspect(): i32 { a.secret() }',
              'no matching function')
        check('selected', '''
import test.a.{Item as A, make as create, count as n, same as identity,}
extern def inspect(): i32 {
  let x: A = create(identity(4))
  if identity(true) { x.value + n } else { 0 }
}
''')
        check('selected_short_alias', '''
import test.a.Item as T
import test.a.make as create
extern def inspect(): i32 { let x: T = create(4); x.value }
''')
        check('wildcard', '''
import test.a.*
extern def inspect(): i32 { let x: Item = make(2); x.value + count }
''')
        check('wildcard_spaced', 'import test.a. *\ndef inspect(): i32 { count }')
        check('wildcard_private', 'import test.a.*\ndef inspect(): i32 { secret() }',
              'no matching function')
        check('private_selection', 'import test.a.secret', 'not public')
        write('compare.yu', """
public struct Key { public value: i32 }
public def ==(a: Key&, b: Key&): bool { a.value == b.value }
""")
        check('selected_operator_alias', """
import test.compare.{Key, == as equal}
extern def inspect(): bool { let a = [Key] {1}; let b = [Key] {2}; equal(a, b) }
""")
        check('selected_operator_short_alias', """
import test.compare.Key
import test.compare.== as equal
extern def inspect(): bool { let a = [Key] {1}; let b = [Key] {2}; equal(a, b) }
""")
        check('repeated_aliases', '''
import test.a as first
import test.a as second
extern def inspect(): i32 { let x: first.Item = second.make(5); x.read() }
''')
        check('conflicting_bindings', 'import test.a.count\nimport test.b.count',
              'conflict')
        write('facade.yu', 'import test.a\npublic def value(): i32 { a.count }')
        check('imports_stay_private', 'import test.facade.a', 'not public')
        check('no_public_import', 'public import test.a', 'import bindings cannot be')
        check('no_implicit_library', 'import a', 'library-qualified')
        check('no_library_namespace', 'import test', 'library-qualified')
        write('directory/leaf.yu', 'public let value: i32 = 1')
        check('no_directory_namespace', 'import test.directory', 'not a source module')
        write('directory.yu', 'public let value: i32 = 3')
        check('real_module_beside_directory', '''
import test.directory
extern def inspect(): i32 { directory.value }
''')
        check('group_requires_module', 'import test.missing.{a}', 'not a source module')
        write('directory_dependency.yu', 'public struct Item { public value: i32 }')
        (library / 'directory_dependency').mkdir()
        check('candidate_directory_dependency', 'import test.directory_dependency.Item')
        dependencies = (output_dir / 'candidate_directory_dependency.ll.d').read_text()
        assert str(library / 'directory_dependency') in dependencies
        write('directory_dependency/Item.yu', 'public let value: i32 = 1')
        check('new_candidate_becomes_ambiguous', 'import test.directory_dependency.Item',
              'ambiguous import')
        check('empty_group', 'import test.a.{}', 'expected imported definition')
        check('bad_alias', 'import test.a.Item as', 'expected identifier')
        check('no_local_import', 'def inspect() { import test.a }', 'invalid in-block statement')
        check('no_local_public', 'def inspect() { public let value = 3 }', 'invalid')

        write('collision.yu', 'public struct Thing { public n: i32 }')
        write('collision/Thing.yu', 'public let answer: i32 = 1')
        check('module_member_ambiguity', 'import test.collision.Thing', 'ambiguous import')
        check('group_does_not_disambiguate', 'import test.collision.{Thing}',
              'ambiguous import')
        check('alias_does_not_disambiguate', 'import test.collision.Thing as T',
              'ambiguous import')
        write('private_collision.yu', 'struct Thing { n: i32 }')
        write('private_collision/Thing.yu', 'public let answer: i32 = 7')
        check('private_member_not_candidate', '''
import test.private_collision.Thing as module
extern def inspect(): i32 { module.answer }
''')
        check('private_group_not_candidate', 'import test.private_collision.{Thing}',
              'not public')
        # A possible competing child module is not actually imported by a
        # selected-definition import, so its own imports cannot form a cycle.
        write('private_collision/Thing.yu', 'import test.private_collision')
        check('private_group_does_not_load_child', 'import test.private_collision.{Thing}',
              'not public')
        # The prefix of a full module path can name the importing module itself.
        # Without a public matching member, this is not a self-import edge.
        write('outer/child.yu', 'public let value: i32 = 4')
        outer = write('outer.yu', '''
import test.outer.child as child
public def value(): i32 { child.value }
''')
        compile_file('module_prefix_is_current_module', outer)

        write('hidden.yu', '''
struct Inner { value: i32 }
public struct Box { inner: Inner, public count: i32 }
public def make(): Box { [] {[] {7}, 3} }
public def read(this: Box&): i32 { this.inner.value }
''')
        check('private_layout_dependency', '''
import test.hidden as h
extern def inspect(): i32 { let box = h.make(); box.read() + box.count }
''')
        check('private_field', '''
import test.hidden as h
def inspect(): i32 { let box = h.make(); box.inner.value }
''', 'field is private')
        check('private_aggregate_empty', 'import test.hidden as h\nvar box: h.Box = [] {}',
              'private fields')
        check('private_aggregate_explicit', 'import test.hidden as h\nvar box = [h.Box] {}',
              'private fields')
        check('private_type_name', 'import test.hidden as h\nvar inner: h.Inner',
              'not public')
        write('leak.yu', 'struct Inner { x: i32 }\npublic def expose(): Inner { [] {} }')
        check('public_signature_leak', 'import test.leak', 'public API exposes a private type')
        write('field_leak.yu', 'struct Inner { x: i32 }\npublic struct Box { public x: Inner }')
        check('public_field_leak', 'import test.field_leak', 'public field exposes a private type')
        write('foreign_type.yu', '''
import test.a as private_name
public def echo(value: private_name.Item): private_name.Item { value }
''')
        check('public_type_from_private_import', '''
import test.foreign_type as api
import test.a
extern def inspect(): i32 { let value: a.Item = api.echo(a.make(4)); value.value }
''')
        check('nominal_modules', '''
import test.a
import test.b
var invalid: a.Item = b.make(1)
''', 'type mismatch')
        check('nominal_local', 'struct A { x: i32 }\nstruct B { x: i32 }\nvar bad: A = [B] {}',
              'type mismatch')
        check('transparent_alias', '''
import test.a
type Alias = a.Item
var valid: Alias = a.make(1)
''')

        write('cycle_a.yu', 'import test.cycle_b')
        write('cycle_b.yu', 'import test.cycle_a')
        check('cycle', 'import test.cycle_a', ['cyclic import', 'test.cycle_a', 'test.cycle_b'])
        self_source = write('self_cycle.yu', 'import test.self_cycle')
        compile_file('self_cycle', self_source, diagnostic='cyclic import')
        write('left.yu', 'import test.a\npublic def value(): i32 { a.count }')
        write('right.yu', 'import test.a\npublic def value(): i32 { a.count }')
        check('diamond', '''
import test.left
import test.right
extern def inspect(): i32 { left.value() + right.value() }
''')
        write('broken_private.yu', 'public let value: i32 = 1\ndef broken() { let x = }')
        check('private_body_is_parsed', 'import test.broken_private', 'invalid value')

        # Registered roots alone determine the primary input's module identity.
        owner = write('identity.yu', 'public def identity(x: i32): i32 { x + 1 }')
        owner_ir = compile_file('owner', owner)
        caller_ir = check('identity_caller', '''
import test.identity as renamed
extern def inspect(): i32 { renamed.identity(3) }
''')
        symbol_pattern = r'@(?:"([^"]+)"|([A-Za-z0-9_.$]+))\('

        def identity_symbol(ir):
            names = {quoted or bare for quoted, bare in re.findall(symbol_pattern, ir)}
            matches = [name for name in names if name.startswith('_Y') and 'identity' in name]
            assert len(matches) == 1, (matches, ir)
            return matches[0]

        symbol = identity_symbol(owner_ir)
        assert identity_symbol(caller_ir) == symbol
        relative_ir = compile_file('relative_owner', Path('identity.yu'),
                                   roots=[Path('.')], cwd=library)
        assert identity_symbol(relative_ir) == symbol
        relocated = work / 'relocated'
        shutil.copytree(library, relocated)
        moved_ir = compile_file('moved_owner', relocated / 'identity.yu', roots=[relocated])
        assert identity_symbol(moved_ir) == symbol
        (library / 'alias.yu').symlink_to(owner)
        alias_ir = check('symlink_module', '''
import test.alias as renamed
extern def inspect(): i32 { renamed.identity(3) }
''')
        assert identity_symbol(alias_ir) == symbol
        linked_root = work / 'linked_library'
        linked_root.symlink_to(library, target_is_directory=True)
        duplicate_ir = compile_file('duplicate_root', owner, roots=[library, linked_root])
        assert identity_symbol(duplicate_ir) == symbol
        unnamed_ir = compile_file('anonymous_owner', owner, roots=[])
        assert 'define internal' in unnamed_ir and identity_symbol(unnamed_ir) != symbol
        # Merely placing a marker next to an entry source does not register it.
        (inputs / 'entry.lib.yu').write_text('')
        (inputs / 'helper.yu').write_text('public let value: i32 = 1\n')
        check('no_ancestor_registration', 'import entry.helper', 'not a source module')
        check('anonymous_imports_external_library', '''
import test.a
extern def inspect(): i32 { a.count }
''')

        blank_input = inputs / 'blank.yu'
        blank_input.write_text('extern def inspect(): i32 { 0 }\n')
        no_marker = work / 'no_marker'
        no_marker.mkdir()
        compile_file('missing_marker', blank_input, [no_marker], 'exactly one .lib.yu')
        two_markers = work / 'two_markers'
        two_markers.mkdir()
        (two_markers / 'one.lib.yu').write_text('')
        (two_markers / 'two.lib.yu').write_text('')
        compile_file('two_markers', blank_input, [two_markers], 'exactly one .lib.yu')
        comment_marker = work / 'comment_marker'
        comment_marker.mkdir()
        (comment_marker / 'comment.lib.yu').write_text('// not whitespace\n')
        compile_file('comment_marker', blank_input, [comment_marker], 'only whitespace')
        invalid_name = work / 'invalid_name'
        invalid_name.mkdir()
        (invalid_name / 'bad-name.lib.yu').write_text('')
        compile_file('invalid_library_name', blank_input, [invalid_name], 'invalid library name')
        compile_file('duplicate_library_name', blank_input, [library, relocated],
                     'duplicate library name')
        compile_file('root_is_not_directory', blank_input, [owner], 'invalid library root')
        compile_file('subdirectory_is_not_root', blank_input, [library / 'directory'],
                     'exactly one .lib.yu')
        nested = library / 'nested'
        nested.mkdir()
        (nested / 'nested.lib.yu').write_text('')
        compile_file('overlapping_roots', blank_input, [library, nested], 'must not overlap')
        compile_file('marker_is_not_source', library / 'test.lib.yu', [library],
                     'marker is not a source module')
        outside = work / 'outside.yu'
        outside.write_text('public let value: i32 = 3\n')
        (library / 'escape.yu').symlink_to(outside)
        check('module_escapes_root', 'import test.escape', 'outside its registered library root')
        unicode_root = work / 'unicode'
        unicode_root.mkdir()
        (unicode_root / '羽.lib.yu').write_text('')
        (unicode_root / '数.yu').write_text('public let 值: i32 = 4\n', encoding='utf-8')
        check('unicode_library', 'import 羽.数\nextern def inspect(): i32 { 数.值 }',
              roots=[unicode_root])

    print(f'module and visibility cases: {count} passed')


if __name__ == '__main__':
    main()
