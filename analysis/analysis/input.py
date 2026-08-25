from pathlib import Path
from analysis.ast import Ast, AstKind, AstId
from analysis.utils import bad_exit
from egglog import Vec


def parse_file(file: Path) -> dict[int, Ast]:

    text = file.read_text()
    lines = text.splitlines()
    # Remove last line if empty.
    if len(lines) > 0 and len(lines[-1]) == 0: lines.pop()

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
        def check_one_id_arg() -> AstId:
            return ast_id(check_one_arg())
        def check_two_id_args() -> tuple[AstId, AstId]:
            arg1_str, arg2_str = check_two_args()
            return ast_id(arg1_str), ast_id(arg2_str)
        def check_many_id_args() -> tuple[AstId, ...]:
            return tuple(
                ast_id(a)
                for a in (
                    args.split(',') if args is not None
                    else []
                )
            )
        # Actual code
        if kind_str == "var":
            return AstKind.var(check_one_arg())
        elif kind_str == "num":
            return AstKind.num(int(check_one_arg()))
        elif kind_str == "add":
            return AstKind.add(*check_two_id_args())
        elif kind_str == "sub":
            return AstKind.sub(*check_two_id_args())
        elif kind_str == "set":
            return AstKind.set(Vec[AstId](*check_many_id_args()))
        elif kind_str == "definition":
            return AstKind.definition(check_one_id_arg())
        elif kind_str == "eq":
            return AstKind.eq(*check_two_id_args())
        else:
            return AstKind.error(f"Unknown kind '{kind_str}'")

    expected_header_row = "id,kind,arguments"

    def parse_row(i: int, line: str, asts: dict[int, Ast]) -> Ast:
        # Declare the error message
        def my_bad_exit(msg: str):
            return bad_exit(f"The {i+1}th row of '{file}' could not be "
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
        bad_exit(f"Given file '{file}' was empty - missing header row")
    header_row = lines.pop(0)
    if header_row != expected_header_row:
        bad_exit(f"Given file '{file}' had an invalid header row - "
                 f"expected '{expected_header_row}', but got '{header_row}'")
    asts: dict[int, Ast] = {}
    for i, line in enumerate(lines):
        parse_row(i, line, asts)

    return asts
