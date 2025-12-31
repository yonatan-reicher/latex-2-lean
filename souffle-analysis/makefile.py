#! /usr/bin/env python3
"""
This python file acts as our Makefile file. This is where we build, compile,
test, run, and everything in between.

First, this script checks for all required shell commands. See
`required_shell_commands` for the full list.
"""

from pathlib import Path
from shutil import which, copy, copytree
from sys import argv
from tempfile import NamedTemporaryFile, TemporaryDirectory
import os
from preprocess import preprocess, backwards


class MyException(Exception):
    """ Class used for errors that we check for """
    pass


# == Constants ==


required_shell_commands = [
    'c++',
    'python3',
    'souffle',
]


help_message = """
USAGE:
    python3 makefile.py [command] <flags>

COMMANDS:
    help - This!
    run - Run the program.
        If the flag `-po`/`--process-output` is given, the outupt will be
        processed via the preprocessing step (A little buggy).
        If the flag `-do=<dir>`/`--directory-output=<dir>` is given, the output will
        be saved to a directory named `dir`.

FLAGS:
    -po / --process-output - Process the output via the preprocessing step.
    -do=<dir> / --directory-output=<dir> - Save the output to directory `dir`.
    -i=<name>:<string> - Input an input relation named `name` with contents `string`.
    -o=<name> / --output=<name> - Save the output relation named `name` to a file.
""".strip()


root = Path('.')
src_dir = root / 'src'
put_error_temp_files_in = root / 'temp/'
souffle_main = 'main.dl'
functors = 'functors.cpp'
output = 'output.txt'


# == Helpers ==


def run_souffle(
    file: Path,
    directory_output: None | Path = None,
) -> bool:
    directory_flag_value = f'"{directory_output}"' if directory_output else '-'
    return 0 == os.system(
        f"souffle {file} -D {directory_flag_value} -F {directory_flag_value} > {output}"
    )


def check_for_shell_commands():
    commands = required_shell_commands
    missing_commands = [cmd for cmd in commands if which(cmd) is None]
    if missing_commands != []:
        raise MyException(
            f"Missing required commands for this project.\n"
            f"Missing commands: {', '.join(missing_commands) }.\n"
            f"\n"
            f"Maybe you need to install them? Or check your Path?\n"
        )


def compile_cpp_to_shared_lib(input: Path, output: Path) -> bool:
    return 0 == os.system(
        "c++ "
        "-std=c++17 "  # Cpp version
        "-c "  # Compile but don't link
        "-fPIC "  # Position independent code, required for shared libraries
        "-shared "  # Create a shared library!
        f"-o {output} "
        f"{input} "
    )


# == Commands ==


def help_command():
    print(help_message)


def is_help():
    return (
        len(argv) == 1
        or argv[1] == 'help'
        or '--help' in argv
    )


def run_command(
    process_output: bool = False,
    additional_inputs: dict[str, list[str]] = {},
    additional_outputs: set[str] = {},
    directory_output: None | Path = None,
):
    with TemporaryDirectory() as temp_dir_name:
        temp_dir_path = Path(temp_dir_name)
        with NamedTemporaryFile() as functors_shared_lib:
            functors_shared_lib_path = Path(functors_shared_lib.name)
            print(f'Compiling {functors} to shared library')
            success = compile_cpp_to_shared_lib(
                src_dir / functors,
                functors_shared_lib_path
            )
            if not success:
                raise MyException(
                    f"Failed to compile {functors}. "
                    "Check the output above for more information."
                )
            print('Preprocessing Soufflé code')
            preprocess(src_dir, temp_dir_path)
            for name in additional_outputs:
                print(f'Adding output flag for relation {name}')
                with (temp_dir_path / souffle_main).open('a') as f:
                    f.write(f'\n.output {name}\n')
            if len(additional_inputs) > 0:
                print('Generating input files')
                for name, contents in additional_inputs.items():
                    (directory_output / f"{name}.facts").write_text('\n'.join(contents))
            print('Running Soufflé code')
            success = run_souffle(
                temp_dir_path / souffle_main,
                directory_output=directory_output,
            )
            if not success:
                put_error_temp_files_in.mkdir(exist_ok=True)
                copy(functors_shared_lib_path, put_error_temp_files_in /
                     functors)
                copytree(temp_dir_path, put_error_temp_files_in,
                         dirs_exist_ok=True)
                raise MyException(
                    "Soufflé failed to run. "
                    "Check the output above for more information. "
                    f"Generated files are in {put_error_temp_files_in.absolute()}. "
                )
            output_text = Path(output).read_text()
            if process_output:
                print('Processing output')
                output_text = backwards(output_text)
            print(output_text)


def unknown_command():
    raise MyException(f"Unknown command: {argv}.")


def get_directory_output_flag_value() -> None | Path:
    try:
        flag = next(a for a in argv if '--directory-output=' in a or '-do=' in a)
    except StopIteration:
        if any(True for a in argv if '--directory-output' in a or '-do' in a):
            raise MyException(
                "The --directory-output/--do flag requires an argument. \n"
                "Usage: --directory-output=<dir> or -do=<dir> \n"
                "Not just --directory-output or -do without an argument, or with a "
                "space before the argument."
            )
        else:
            return None
    else:
        name = flag.split('=', maxsplit=1)[1]
        if name.strip() == '':
            raise MyException("directory-output flag requires a non-empty argument. \n")
        path = Path(name)
        if not path.exists():
            raise MyException(f"The path {name} does not exist.")
        if not path.is_dir():
            raise MyException(f"The path {name} exists, but is not a directory.")
        return path


def parse_input_flags():
    def is_flag(arg: str):
        return arg.startswith('-i=') or arg.startswith('--input=')

    def parse(flag: str):
        content = flag[flag.index('=') + 1:]
        try:
            name, string = content.split(':', maxsplit=1)
            string = string.splitlines()
        except ValueError:
            raise MyException(
                "Input flag requires both a name and string separated by a colon. \n"
                "Usage: -i=<name>:<string> or --input=<name>:<string> \n"
            )
        return (name, string)

    return { k: v for k, v in [ parse(a) for a in argv if is_flag(a) ] }


def parse_output_flags():
    def is_flag(arg: str):
        return arg.startswith('-o=') or arg.startswith('--output=')

    def parse(flag: str):
        content = flag[flag.index('=') + 1:]
        return content

    return { parse(a) for a in argv if is_flag(a) }


def main():
    check_for_shell_commands()
    # Help should be checked first
    if is_help():
        help_command()
        return
    additional_inputs = parse_input_flags()
    additional_outputs = parse_output_flags()
    if argv[1] == 'run':
        process_output = '--process-output' in argv or '-po' in argv
        directory_output = get_directory_output_flag_value()
        run_command(
            process_output=process_output,
            additional_inputs=additional_inputs,
            additional_outputs=additional_outputs,
            directory_output=directory_output,
        )
    else:
        unknown_command()


if __name__ == '__main__':
    main()
