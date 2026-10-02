"""Exercise Unicode 17.0.0 XID identifiers and strict UTF-8 token decoding."""
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
        'declarations': '''
struct 点 { 横: i32, 纵: i32 }
enum 颜色 { 红, 蓝 }
type 整数 = i32
def 相加(左: 整数, 右: 整数): 整数 { let 和 = 左 + 右; 和 }
def 使用(值: 点): 整数 { 相加(值.横, 值.纵) }
def 色彩(): 颜色 { 颜色.红 }
''',
        'underscore': 'def _f(_值: i32): i32 { let _ = _值; _ }',
        'spelling_preserved': 'def é(): i32 { 1 }\ndef e\u0301(): i32 { 2 }\n'
                              'def total(): i32 { é() + e\u0301() }',
        'literal_and_comment': 'def f(): i32 { "中文😀"; 0 } // 😀\n/* 😀 */',
    }
    # XID_Start includes letters across planes, Other_ID_Start exceptions, and
    # Tai Yo added in Unicode 17.0. It is not equivalent to isalpha/iswalnum.
    starts = ['é', 'Δ', '变量', '\U00010400', '\U00030000', '\u2118', '\u212e',
              '\U0001e6c0']
    for index, name in enumerate(starts):
        accepted[f'start_{index}'] = f'def {name}(值: i32): i32 {{ 值 }}\n'
        accepted[f'start_{index}'] += f'def check(): i32 {{ {name}(7) }}'
    # Combining marks, non-ASCII digits, connector punctuation, Other_ID_Continue,
    # joining controls and variation selectors all belong to XID_Continue.
    continues = ['\u0301', '\u0661', '\u203f', '\u00b7', '\u200c', '\ufe0f']
    for index, suffix in enumerate(continues):
        name = 'value' + suffix
        accepted[f'continue_{index}'] = f'def f({name}: i32): i32 {{ {name} }}'

    # U+037A and U+309B satisfy ID_Start but are excluded from XID_Start.
    invalid_starts = ['\u0301x', '\u0661x', '\u203fx', '\u00b7x', '\u200cx',
                      '\u037ax', '\u309bx', '😀', '€', '\u00a0x']
    rejected = {
        f'invalid_start_{i}': f'var {name}: i32'.encode('utf-8')
        for i, name in enumerate(invalid_starts)
    }
    for index, char in enumerate(['😀', '€', '\u037a', '\u309b', '\u202e']):
        rejected[f'invalid_continue_{index}'] = f'var a{char}: i32'.encode('utf-8')

    malformed = {
        'stray_continuation': b'\x80',
        'overlong_two': b'\xc0\xaf',
        'overlong_three': b'\xe0\x80\xaf',
        'overlong_four': b'\xf0\x80\x80\xaf',
        'surrogate': b'\xed\xa0\x80',
        'too_large': b'\xf4\x90\x80\x80',
        'invalid_lead': b'\xf5\x80\x80\x80',
        'ff': b'\xff',
        'wrong_continuation': b'\xe4A',
        'incomplete_two': b'\xc2',
        'incomplete_three': b'\xe4\xb8',
        'incomplete_four': b'\xf0\x90\x80',
    }
    for name, value in malformed.items():
        rejected[name] = b'var ' + value + b': i32'
        rejected[name + '_after_id'] = b'var valid' + value + b': i32'
        rejected[name + '_at_eof'] = b'var ' + value

    with tempfile.TemporaryDirectory(dir=args.work_dir, prefix='identifiers-') as directory:
        work = Path(directory)

        def compile(name, source, definitions=(), output='llvm'):
            path = work / f'{name}.yu'
            path.write_bytes(source.encode('utf-8') if isinstance(source, str) else source)
            command = [str(args.yuc.resolve()), str(path), '-ot', output]
            for definition in definitions:
                command += ['-D', definition]
            return subprocess.run(command, capture_output=True, text=True,
                                  errors='replace', timeout=10)

        for name, source in accepted.items():
            result = compile(name, source)
            assert result.returncode == 0, (name, result.stderr)
        for name, source in rejected.items():
            result = compile(name, source)
            assert result.returncode == 1, (name, result.returncode, result.stderr)
            diagnostic = 'invalid UTF-8' if name in malformed or any(
                name == key + suffix for key in malformed
                for suffix in ('_after_id', '_at_eof')) else 'invalid Unicode'
            assert diagnostic in result.stderr, (name, result.stderr)

        macros = [
            ('whole_name', 'def f(): i32 { 常量 }', ['常量=7'], 'def f(): i32 { 7 }'),
            ('continuation', 'def f(): i32 { 数\u0301 }', ['数\u0301=7'],
             'def f(): i32 { 7 }'),
            ('replacement', 'var 名称: 类型', ['名称=中文', '类型=i32'], 'var 中文: i32'),
            ('boundary', 'def f(): i32 { 宏+2 }', ['宏=1'], 'def f(): i32 { 1+2 }'),
            ('prefix', 'var 常量_suffix: i32', ['常量=7'], 'var 常量_suffix: i32'),
            ('nonrecursive', '声明\ndef f(): i32 { 甲 }', ['声明=var 乙: i32', '甲=乙', '乙=7'],
             'var 乙: i32\ndef f(): i32 { 乙 }'),
        ]
        for name, source, definitions, expected in macros:
            actual = compile(name, source, definitions, output='ast')
            reference = compile(name + '_reference', expected, output='ast')
            assert actual.returncode == reference.returncode == 0, (name, actual.stderr, reference.stderr)
            assert actual.stdout == reference.stdout, (name, actual.stdout, reference.stdout)

        for name in invalid_starts + ['x😀', 'a\u037a']:
            result = compile('invalid_macro', 'var value: i32', [name + '=1'])
            assert result.returncode == 1, (name, result.stderr)
            assert 'invalid macro definition' in result.stderr, (name, result.stderr)
        for name, value in malformed.items():
            result = compile('malformed_macro', 'var value: i32', [value + b'=1'])
            assert result.returncode == 1, (name, result.stderr)
            assert 'invalid macro definition' in result.stderr, (name, result.stderr)

        # Unicode module names, imported declarations and definitions share the
        # same lexical rules, including macro expansion in the imported source.
        (work / '数学.yu').write_text('public type 整数 = 类型\n', encoding='utf-8')
        result = compile('import', 'import 数学\nvar 值: 整数', ['类型=i32'])
        assert result.returncode == 0, result.stderr

    print(f'PASS: {len(accepted)} accepted, {len(rejected)} rejected, Unicode macros/imports')


if __name__ == '__main__':
    main()
