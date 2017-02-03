import os
import sys
import json
import distutils.core
import distutils.util


def magic_extension(caller_file, *, name, sources, headers):
    '''See examples/swig_demo/__init__.py.'''

    # contextlib.redirect_stdout is not enough because spawned processes
    # (compilers and SWIG) can spam to stdout as well
    try:
        stdout_backup = os.dup(sys.stdout.fileno())
        os.dup2(sys.stderr.fileno(), sys.stdout.fileno())
        build_extension(caller_file, name, sources, headers)
    finally:
        os.dup2(stdout_backup, sys.stdout.fileno())


def build_extension(caller_file, name, sources, headers):
    release = distutils.util.strtobool(os.getenv('TBD_RELEASE', '0'))
    if release:
        extra_compile_args = [
            '-ggdb', '-std=c++11',
            '-O2']
        undef_macros = ['NDEBUG']  # want assertions even in the release build?
    else:
        extra_compile_args = [
            '-ggdb', '-std=c++11',
            '-O0', '-D_GLIBCXX_DEBUG', '-D_GLIBCXX_DEBUG_PEDANTIC']
        undef_macros = ['NDEBUG']  # want assertions

    caller_file = os.path.abspath(caller_file)
    this_module = os.path.abspath(__file__)
    config_path = os.path.join(
        os.path.dirname(this_module),
        '_compiler_config.json')

    cur_dir = os.getcwd()
    try:
        os.chdir(os.path.dirname(caller_file))

        # Distutils looks at the timestamps to determine whether the extension
        # should be rebuilt. We want it rebuilt when compiler options change,
        # even if no source file is touched.
        config = '// generated by swig_utils.py\n' + json.dumps(dict(
            CC=os.getenv('CC'),
            extra_compile_args=extra_compile_args,
            undef_macros=undef_macros), sort_keys=True, indent='  ')
        update_file_content_if_changed(config_path, config)

        distutils.core.setup(
            name=name,
            py_modules=[name],
            ext_modules=[
                distutils.core.Extension('_' + name,
                    sources,
                    depends=headers + [caller_file, config_path, this_module],
                    swig_opts=['-c++'],
                    extra_compile_args=extra_compile_args,
                    undef_macros=undef_macros,
                ),
            ],
            script_args=['--quiet', 'build_ext', '--inplace']
        )
    finally:
        os.chdir(cur_dir)


def update_file_content_if_changed(filename, content):
    if os.path.exists(filename):
        with open(filename) as f:
            old_content = f.read()
    else:
        old_content = None
    if content != old_content:
        with open(filename, 'w') as f:
            f.write(content)
