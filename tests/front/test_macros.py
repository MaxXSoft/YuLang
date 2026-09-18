"""Check command-line, single-level identifier expansion through the compiler."""
import argparse
from pathlib import Path
import re
import subprocess
import tempfile

parser = argparse.ArgumentParser()
parser.add_argument('--yuc', type=Path, required=True)
parser.add_argument('--work-dir', type=Path, required=True)
args = parser.parse_args()
args.work_dir.mkdir(parents=True, exist_ok=True)

with tempfile.TemporaryDirectory(dir=args.work_dir, prefix='macros-') as directory:
    work = Path(directory).resolve()

    def compile(source, definitions=(), option='-D'):
        path = work / 'main.yu'
        path.write_text(source)
        command = [str(args.yuc.resolve()), str(path), '-ot', 'ast']
        for definition in definitions:
            command += [option, definition]
        return subprocess.run(command, capture_output=True, text=True, timeout=10)

    cases = [
        ('expression', 'def f(): i32 { VALUE*3 }', ['VALUE=1 + 2'],
         'def f(): i32 { 1 + 2*3 }'),
        ('type_and_name', 'var NAME: TYPE', ['NAME=value', 'TYPE=i32'],
         'var value: i32'),
        ('adjacent', 'def f(): i32 { X+X }', ['X=2'],
         'def f(): i32 { 2+2 }'),
        ('call', 'def f(): i32 { 1 }\ndef g(): i32 { FN() }', ['FN=f'],
         'def f(): i32 { 1 }\ndef g(): i32 { f() }'),
        ('operator_boundary', 'def f(): i32 { 1 OP+2 }', ['OP=+'],
         'def f(): i32 { 1 + +2 }'),
        ('empty', 'var REMOVE value: i32', ['REMOVE='], 'var value: i32'),
        ('empty_eof', 'var value: i32 REMOVE', ['REMOVE='], 'var value: i32'),
        ('whole_file', 'DECL', ['DECL=var value: i32'], 'var value: i32'),
        ('spaces', 'var REMOVE value: TYPE', ['REMOVE=  ', 'TYPE=i32  '],
         'var value: i32'),
        ('self', 'def f(A: i32): i32 { A }', ['A=A'],
         'def f(A: i32): i32 { A }'),
        ('whole_identifier', 'var XXX_suffix: i32 // XXX\n/* XXX */', ['XXX=0'],
         'var XXX_suffix: i32'),
        ('literal', 'def f(): i32 { "XXX"; \'X\'; 0 }', ['XXX=0', 'X=0'],
         'def f(): i32 { "XXX"; \'X\'; 0 }'),
        ('equals', 'def f(): bool { TEST }', ['TEST=1 == 1'],
         'def f(): bool { 1 == 1 }'),
        ('duplicate', 'var value: TYPE', ['TYPE=bool', 'TYPE=i32'],
         'var value: i32'),
        ('keyword', 'var value: i32', ['var=bad', 'i32=bad'], 'var value: i32'),
        ('line_comment', 'var value: TYPE', ['TYPE=i32 // comment'],
         'var value: i32'),
        ('comment_boundary', 'def f(): i32 { X+2 }', ['X=1 // comment'],
         'def f(): i32 { 1+2 }'),
        ('block_comment', 'var value: TYPE', ['TYPE=/* comment */ i32'],
         'var value: i32'),
        ('multiline', 'DECL var second: i32', ['DECL=var first: i32\n'],
         'var first: i32\nvar second: i32'),
        ('semicolon', 'DECL var second: i32', ['DECL=var first: i32;'],
         'var first: i32; var second: i32'),
        ('string_replacement', 'def f(): i32 { TEXT; 0 }', ['TEXT="hello world"'],
         'def f(): i32 { "hello world"; 0 }'),
    ]
    # Declare B through another replacement to test A=B, B=1 without replacing
    # the declaration's name in the original source.
    cases.append(('chain', 'DECL\ndef f(): i32 { A }',
                  ['DECL=var B: i32', 'A=B', 'B=1'],
                  'var B: i32\ndef f(): i32 { B }'))
    for name, source, definitions, expected in cases:
        reference = compile(expected)
        assert reference.returncode == 0, (name, reference.stderr)
        actual = compile(source, definitions)
        assert actual.returncode == 0, (name, actual.stderr)
        assert actual.stdout == reference.stdout, (name, actual.stdout, reference.stdout)

    actual = compile('def f(): i32 { X }', ['X=1 + 2'], option='--define')
    reference = compile('def f(): i32 { 1 + 2 }')
    assert actual.returncode == reference.returncode == 0, actual.stderr
    assert actual.stdout == reference.stdout

    (work / 'leaf.yu').write_text('public type Number = TYPE\n')
    actual = compile('import leaf\nvar value: Number', ['TYPE=i32'])
    (work / 'leaf.yu').write_text('public type Number = i32\n')
    reference = compile('import leaf\nvar value: Number')
    assert actual.returncode == reference.returncode == 0, actual.stderr
    assert actual.stdout == reference.stdout

    for definition in ['', 'MISSING', '=1', '1BAD=1', 'BAD-NAME=1', 'BAD NAME=1']:
        result = compile('var value: i32', [definition])
        assert result.returncode == 1, (definition, result.stderr)
        assert 'invalid macro definition' in result.stderr, result.stderr

    for replacement, diagnostic in [('/*', 'comment unclosed'),
                                    ('"abc', 'expected'), ("'", 'expected'),
                                    ("'x", 'expected')]:
        result = compile('def f(): i32 { X; 0 }', ['X=' + replacement])
        assert result.returncode == 1, (replacement, result.stderr)
        assert diagnostic in result.stderr, result.stderr

    # Expansion newlines must not change diagnostics in the original file.
    source = 'REMOVE\ndef f(): i32 { unknown }\n'
    reference = compile(source, ['REMOVE='])
    actual = compile(source, ['REMOVE=\n\n/* comment */\n'])
    assert actual.returncode == reference.returncode == 1
    assert actual.stderr == reference.stderr, (actual.stderr, reference.stderr)
    assert re.search(r'main\.yu:.*2:', actual.stderr), actual.stderr
