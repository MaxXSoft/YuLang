"""Check trailing commas in parameter, argument, and function-type lists."""
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
        'formal': ('def f(x: i32,): i32 { x }', 'def f(x: i32): i32 { x }'),
        'multiple': ('def f(x: i32, y: i32,): i32 { x + y }',
                     'def f(x: i32, y: i32): i32 { x + y }'),
        'call': ('def f(x: i32): i32 { x }\nlet n = f(42,)',
                 'def f(x: i32): i32 { x }\nlet n = f(42)'),
        'dot': ('def f(x: i32, y: i32): i32 { x + y }\nlet n = (1).f(2,)',
                'def f(x: i32, y: i32): i32 { x + y }\nlet n = (1).f(2)'),
        'type': ('extern declare f: (i32,): i32\nlet n = f(1)',
                 'extern declare f: (i32): i32\nlet n = f(1)'),
        'nested': ('def g(f: (i32,): i32,): i32 { f(1,) }',
                   'def g(f: (i32): i32): i32 { f(1) }'),
        'multiline': ('def g(\nx: i32,\ny: i32,\n): i32 { x + y }\n'
                      'let n = g(\n1,\n2,\n)',
                      'def g(x: i32, y: i32): i32 { x + y }\nlet n = g(1, 2)'),
        'empty': ('def f() {}\nextern declare g: ()\ndef h() { f(); g() }',
                  'def f() {}\nextern declare g: ()\ndef h() { f(); g() }'),
    }
    rejected = {
        'empty_formal': 'def f(,) {}',
        'empty_call': 'def f() {}\ndef g() { f(,) }',
        'empty_type': 'extern declare f: (,)',
        'double_formal': 'def f(x: i32,,) {}',
        'double_call': 'def f(x: i32) {}\ndef g() { f(1,,) }',
        'double_type': 'extern declare f: (i32,,)',
        'leading_formal': 'def f(,x: i32) {}',
        'leading_call': 'def f(x: i32) {}\ndef g() { f(,1) }',
        'leading_type': 'extern declare f: (,i32)',
        'missing_formal': 'def f(x: i32 y: i32) {}',
        'missing_type': 'extern declare f: (i32 i32)',
    }
    with tempfile.TemporaryDirectory(dir=args.work_dir, prefix='parameters-') as directory:
        work = Path(directory)
        source = work / 'input.yu'
        output = work / 'output.ll'

        def compile(text):
            source.write_text(text + '\n')
            return subprocess.run([str(args.yuc), str(source), '-ot', 'llvm',
                                   '-o', str(output)], capture_output=True, text=True,
                                  timeout=10)

        for name, pair in accepted.items():
            results = []
            for text in pair:
                result = compile(text)
                assert result.returncode == 0, (name, result.stderr)
                results.append(output.read_text())
            assert results[0] == results[1], (name, 'trailing comma changed IR')
        for name, text in rejected.items():
            result = compile(text)
            assert result.returncode == 1, (name, result.returncode, result.stderr)
            assert 'error:' in result.stderr, (name, result.stderr)
    print(f'PASS: {len(accepted)} IR equivalence pairs, {len(rejected)} rejected lists')


if __name__ == '__main__':
    main()
