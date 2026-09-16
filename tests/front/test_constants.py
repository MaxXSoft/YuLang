"""Reject nonconstant dimensions/enum values and accept valid constants."""
import argparse
from pathlib import Path
import subprocess
import tempfile

parser = argparse.ArgumentParser()
parser.add_argument('--yuc', type=Path, required=True)
parser.add_argument('--work-dir', type=Path, required=True)
args = parser.parse_args()
args.work_dir.mkdir(parents=True, exist_ok=True)
cases = [
    ('array', 'def f(n: i32): i32 { var a: i32[n]; 0 }', 'invalid array length'),
    ('zero', 'var a: i32[0]', 'invalid array length'),
    ('enum', 'extern declare getnum: (): i32\nenum E { A = getnum() }',
     'enumeration value must be a constant integer'),
    ('valid', 'enum E { A = 2, B }\nvar a: i32[2 + 1]', None),
]
with tempfile.TemporaryDirectory(dir=args.work_dir, prefix='constants-') as directory:
    work = Path(directory)
    for name, source, diagnostic in cases:
        path = work / f'{name}.yu'
        path.write_text(source + '\n')
        result = subprocess.run([str(args.yuc), str(path), '-ot', 'llvm', '-o',
                                 str(work / f'{name}.ll')], capture_output=True, text=True)
        if diagnostic is None:
            assert result.returncode == 0, result.stderr
        else:
            assert result.returncode == 1, (name, result.returncode, result.stderr)
            assert diagnostic in result.stderr, (name, result.stderr)
