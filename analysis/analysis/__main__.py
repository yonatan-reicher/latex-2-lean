from analysis.ast import Ast, AstKind
from analysis.my_egraph import MyEGraph
from analysis.utils import bad_exit
from dataclasses import dataclass
from pathlib import Path
from sys import argv
from typing import Any

@dataclass(frozen=True, slots=True)
class CliArguments:
    input_file: Path
    @staticmethod
    def parse() -> CliArguments:
        """ Constructs the object from `sys.argv` """
        if len(argv) != 2:
            bad_exit("Usage: python -m egglog <input_file>")
        return CliArguments(input_file=Path(argv[1]))

# CLI arguments
args = CliArguments.parse()
if not args.input_file.exists():
    bad_exit(f"Given file '{args.input_file}' was not found")

# Read file
text = args.input_file.read_text()
lines = text.splitlines()

class KindParseError(RuntimeError): pass

def parse_kind(kind_str: str, args: str | None) -> AstKind:
    def check_no_args() -> None:
        if args is not None:
            raise KindParseError(
                f"The '{kind_str}' kind must not have any arguments, but "
                f"'{args}' were given."
            )
    def check_one_arg() -> str:
        if args is None:
            raise KindParseError(
                f"The '{kind_str}' kind must have an argument, but none were "
                f"given."
            )
        return args
    if kind_str == "var":
        return AstKind.var(check_one_arg())
    elif kind_str == "num":
        return AstKind.num(int(check_one_arg()))
    else:
        return AstKind.error(f"Unknown kind '{kind_str}'")

def parse_row(i: int, line: str) -> Ast:
    cells = line.split(' ', 1)
    if len(cells) == 0: 
        bad_exit(f"The {i+1}th row of '{args.input_file}' did not start with a "
                 "node kind - the first word of a line should specify the kind "
                 "of node, and everything after it are arguments for the node")
    kind_str = cells[0]
    args_str = cells[1] if len(cells) == 2 else None
    kind = parse_kind(kind_str, args_str)
    return Ast(1, kind) # TODO: Maybe take id explicitly?

# Validate rows
split_lines = [parse_row(i, line) for i, line in enumerate(lines)]

print("🦮")

print(split_lines)
