from analysis.ast import Ast, AstKind, AstId, AstIdLike, ast_rules
from analysis.my_egraph import MyEGraph
from analysis.utils import bad_exit
from dataclasses import dataclass
from egglog import run
from pathlib import Path
from sys import argv
from typing import Any, Callable

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

def ast_id(s: str) -> AstId:
    try:
        return AstId(int(s))
    except ValueError:
        raise KindParseError(f"Could not parse '{s}' as an AST identifier")

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
    def check_two_args() -> tuple[str, str]:
        if args is None:
            raise KindParseError(
                f"The '{kind_str}' kind must have two arguments, but none were "
                f"given."
            )
        split_args = args.split(',', 1)
        if len(split_args) != 2:
            raise KindParseError(
                f"The '{kind_str}' kind must have two arguments, but only "
                f"'{args}' were given."
            )
        return split_args[0], split_args[1]
    def check_two_id_args() -> tuple[AstId, AstId]:
        arg1_str, arg2_str = check_two_args()
        return ast_id(arg1_str), ast_id(arg2_str)
    # Actual code
    if kind_str == "var":
        return AstKind.var(check_one_arg())
    elif kind_str == "num":
        return AstKind.num(int(check_one_arg()))
    elif kind_str == "add":
        return AstKind.add(*check_two_id_args())
    elif kind_str == "sub":
        return AstKind.sub(*check_two_id_args())
    else:
        return AstKind.error(f"Unknown kind '{kind_str}'")

expected_header_row = "id,kind,arguments"

def parse_row(i: int, line: str, asts: dict[int, Ast]) -> Ast:
    # Declare the error message
    def my_bad_exit(msg: str):
        return bad_exit(f"The {i+1}th row of '{args.input_file}' could not be "
                        f"parsed - was not of the form '{expected_header_row}' "
                        f"- {msg}\n"
                        f"Was: '{line}'")
    cells = line.split(',', 2)
    n_cells = len(cells)
    if n_cells == 0: my_bad_exit("row was empty")
    try:
        id = int(cells[0])
    except ValueError:
        my_bad_exit(f"id cell was not an integer (was '{cells[0]}')")
    if n_cells == 1: my_bad_exit("row had only a single cell")
    assert n_cells in [2, 3]
    kind_str = cells[1]
    args_str = cells[2] if len(cells) > 2 else None
    kind = parse_kind(kind_str, args_str)
    ast = Ast(id, kind)
    asts[id] = ast
    return ast

# Validate rows
if len(lines) == 0:
    bad_exit(f"Given file '{args.input_file}' was empty - missing header row")
header_row = lines.pop(0)
if header_row != expected_header_row:
    bad_exit(f"Given file '{args.input_file}' had an invalid header row - "
             f"expected '{expected_header_row}', but got '{header_row}'")
asts: dict[int, Ast] = {}
ast_list = [parse_row(i, line, asts) for i, line in enumerate(lines)]

print("🦮")

print(ast_list)

egraph = MyEGraph()
for a in ast_list: egraph.register(a)
for r in ast_rules: egraph.register(r)
egraph.run(run().saturate())
egraph.display()
