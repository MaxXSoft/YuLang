"""Check depfile contents, failure handling, and Make/Ninja incremental builds."""
import argparse
import shutil
import subprocess
import tempfile
import time
from pathlib import Path

parser = argparse.ArgumentParser()
parser.add_argument('--yuc', type=Path, required=True)
parser.add_argument('--work-dir', type=Path, required=True)
args = parser.parse_args()
yuc = args.yuc.resolve()
args.work_dir.mkdir(parents=True, exist_ok=True)


def run(command, cwd, success=True):
    result = subprocess.run(command, cwd=cwd, capture_output=True, text=True)
    assert (result.returncode == 0) == success, result.stdout + result.stderr
    return result


with tempfile.TemporaryDirectory(dir=args.work_dir, prefix='dependencies-') as directory:
    work = Path(directory).resolve()
    (work / 'main.yu').write_text('import direct\nimport leaf\nvar value: i32\n')
    (work / 'direct.yu').write_text('public import leaf\n')
    (work / 'leaf.yu').write_text('public type Number = i32\n')
    (work / 'unused.yu').write_text('public type Unused = i32\n')
    compile_cmd = [str(yuc), 'main.yu', '-ot', 'obj', '-o', 'main.o']
    run(compile_cmd, work)
    depfile = work / 'main.o.d'
    assert not depfile.exists()
    run(compile_cmd + ['-MD'], work)
    original = depfile.read_text()
    rules = original.replace('\\\n', '').splitlines()
    assert rules[0].split() == ['main.o:', str(work / 'main.yu'),
                                str(work / 'direct.yu'), str(work / 'leaf.yu')]
    assert [line for line in rules[1:] if line] == [
        f'{work / "direct.yu"}:', f'{work / "leaf.yu"}:']
    run(compile_cmd + ['-MD'], work)
    assert depfile.read_text() == original
    result = run([str(yuc), 'main.yu', '-ot', 'llvm', '-MD'], work, False)
    assert '-MD requires' in result.stderr
    (work / 'leaf.yu').unlink()
    run(compile_cmd + ['-MD'], work, False)
    assert depfile.read_text() == original
    (work / 'leaf.yu').write_text('public type Number = i32\n')
    depfile.unlink()
    depfile.mkdir()
    run(compile_cmd + ['-MD'], work, False)
    assert depfile.is_dir()
    assert not list(work.glob('main.o.d.tmp-*'))
    depfile.rmdir()

    # Exercise CMake's actual depfile consumers, including the extra empty rules.
    for generator, tool in [('Unix Makefiles', 'make'), ('Ninja', 'ninja')]:
        if not shutil.which(tool):
            continue
        project = work / tool
        project.mkdir()
        (project / 'main.yu').write_text('import direct\nvar value: i32\n')
        (project / 'direct.yu').write_text('public import leaf\n')
        (project / 'leaf.yu').write_text('public type Number = i32\n')
        (project / 'unused.yu').write_text('public type Unused = i32\n')
        (project / 'CMakeLists.txt').write_text(f'''
cmake_minimum_required(VERSION 3.28)
project(DepfileTest NONE)
set(object "${{CMAKE_CURRENT_BINARY_DIR}}/main.o")
add_custom_command(OUTPUT "${{object}}"
  COMMAND "{yuc}" "${{CMAKE_CURRENT_SOURCE_DIR}}/main.yu" -o "${{object}}" -MD
  DEPENDS "${{CMAKE_CURRENT_SOURCE_DIR}}/main.yu"
  DEPFILE "${{object}}.d" VERBATIM)
add_custom_target(check ALL DEPENDS "${{object}}")
''')
        run(['cmake', '-S', '.', '-B', 'build', '-G', generator], project)
        build = ['cmake', '--build', 'build']
        run(build, project)
        obj = project / 'build/main.o'
        stamp = obj.stat().st_mtime_ns
        run(build, project)
        assert obj.stat().st_mtime_ns == stamp, generator
        for name, rebuild in [('unused', False), ('leaf', True), ('direct', True)]:
            time.sleep(1.1)
            with (project / f'{name}.yu').open('a') as source:
                source.write('\n// dependency change\n')
            run(build, project)
            current = obj.stat().st_mtime_ns
            assert (current != stamp) == rebuild, (generator, name)
            stamp = current
        # A removed import may still appear in the previous depfile.
        time.sleep(1.1)
        (project / 'direct.yu').write_text('// no longer imports leaf\n')
        (project / 'leaf.yu').unlink()
        run(build, project)
        assert obj.stat().st_mtime_ns != stamp, generator
        assert 'leaf.yu' not in (project / 'build/main.o.d').read_text()
