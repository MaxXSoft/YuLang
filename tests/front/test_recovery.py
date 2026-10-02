"""Check syntax/lexical recovery, source boundaries, and failed output suppression."""
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
    cases = {
        'block': ('def f() {\n let a = ;\n let b = ;\n}\n'
                  'def g() { let c = ; }', 3),
        'next_line': ('def f() {\n let a =\n let b =\n let c = ;\n}', 3),
        'top_level': ('let a = ;\nlet b = ;\nlet c = ;', 3),
        'types': ('declare a: ;\ntype B = ;\ndef f() { declare c: ; type D = ; }', 4),
        'bad_header': ('def bad(x i32) {\n let hidden = 1\n}\n'
                       'let a = ;\ndef next() { let b = ; }', 3),
        'nested': ('def f() {\n if true { let a = ; let b = ; }\n'
                   'let c = ;\n}\nlet d = ;', 4),
        'stray_braces': ('}\n}\nlet a = ;', 3),
        'lexical': ('def f() { let a = 18446744073709551616;\n'
                    'let b = 18446744073709551616;\n let c = ; }', 3),
        'escapes': ('def f() { let a = "\\q";\nlet b = "\\q";\nlet c = ; }', 3),
        'string_newline': ('def f() { let a = "\n let b = ;\n}', 2),
        'missing_close': ('def f() { let a = ;\nlet b = ;', 3),
        'eof_expr': ('let a = ;\nlet b =', 2),
        'parens': ('def f() { let a = (1 + );\nlet b = ; }', 2),
        'pointer': ('declare a: i32 **!\nlet b = ;', 2),
        'later_semantic': ('def f() { let a = ; }\ndef g(): i32 { missing_name }', 2),
    }
    with tempfile.TemporaryDirectory(dir=args.work_dir, prefix='recovery-') as directory:
        work = Path(directory)
        source = work / 'input.yu'

        def compile(text, name, count, *flags, mode='llvm'):
            source.write_bytes(text if isinstance(text, bytes) else (text + '\n').encode('utf-8'))
            output = work / f'{name}.{mode}'
            result = subprocess.run([str(args.yuc), str(source), '-ot', mode,
                                     '-o', str(output), *flags], capture_output=True,
                                    text=True, timeout=10)
            errors = re.sub(r'\x1b\[[0-9;]*m', '', result.stderr)
            assert result.returncode == 1, (name, result.returncode, errors)
            assert errors.count('error:') >= count, (name, errors)
            assert errors.count('error:') <= count + 3, (name, 'cascading errors', errors)
            assert not output.exists() or not output.read_bytes(), (name, 'partial output')
            return errors

        for name, (text, count) in cases.items():
            compile(text, name, count)
        compile(b'var \xed\xa0\x80: i32\nvar \xf4\x90\x80\x80: i32\nlet c = ;\n',
                'utf8_recovery', 3)
        # Recover imported syntax errors and restore the importing source's lexer.
        (work / 'broken.yu').write_text('public declare a: ;\npublic type B = ;\n'
                                       'public declare good: ()\n')
        errors = compile('import broken\ndef f() { good(); let c = ; }\n'
                         'def g() { missing_name }', 'import', 4)
        assert 'broken.yu:' in errors and 'input.yu:' in errors, errors
        assert 'missing_name' in errors, errors
        # Good exports from a partially invalid module remain available for analysis.
        errors = compile('import broken\ndef f() { good() }', 'import_exports', 2)
        assert 'not been defined' not in errors, errors
        errors = compile('extern var a: i32 = 0\ndef f() { missing_name }',
                         'werror', 2, '-Werror')
        assert 'missing_name' in errors and "cannot be 'extern'" in errors, errors
        for mode in ('ast', 'yuir', 'asm', 'obj'):
            compile('def valid() {}\ndef bad() { missing_name }',
                    f'output_{mode}', 1, '-MD', mode=mode)
            assert not (work / f'output_{mode}.{mode}.d').exists()
        # Empty/truncated constructs must terminate without assertion failures.
        for index, text in enumerate(('def', 'def f(', 'def f() {', 'type T = (',
                                      '/*', 'let s = "', 'import', 'public')):
            compile(text, f'truncated_{index}', 1)
        compile(b'let value = "', 'string_eof', 1)
    print(f'PASS: {len(cases)} recovery cases, imports, -Werror, output modes, and EOF')


if __name__ == '__main__':
    main()
