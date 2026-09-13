import analysis.check_egglog
import analysis.generate_bin_op

from analysis.ast import Ast, AstId, ast_to_id
from analysis.input import parse_file
from analysis.my_egraph import MyEGraph
from analysis.rules import all as all_rules
from analysis.utils import bad_exit
from dataclasses import dataclass
from egglog import Unit, function, run
from pathlib import Path
from sys import argv

analyses = frozenset({"is_finite", "used_as_finite"})

USAGE_MSG = f"""
Usage: python -m analysis [--display] <input_file> {' '.join(f"{a}=<path>" for a in analyses)}
""".strip()

@dataclass(frozen=True, slots=True)
class OutputFiles:
    is_finite: Path
    used_as_finite: Path

@dataclass(frozen=True, slots=True)
class CliArguments:
    display: bool
    input_file: Path
    output_files: OutputFiles
    @staticmethod
    def parse() -> CliArguments:
        """ Constructs the object from `sys.argv` """
        args = argv[1:]
        display = False
        if '--display' in args:
            args = [ a for a in args if a != '--display' ]
            display = True
        n_expected_args = 1 + len(analyses)
        if len(args) != n_expected_args: bad_exit(USAGE_MSG)
        parsed = {}
        for arg in args[1:]:
            splot = arg.split('=', maxsplit=1)
            if len(splot) != 2: bad_exit(USAGE_MSG)
            name, path = splot
            parsed[name] = Path(path)
        if frozenset(parsed.keys()) != analyses: bad_exit(USAGE_MSG)
        return CliArguments(
            display=display,
            input_file=Path(args[0]),
            output_files=OutputFiles(
                is_finite=parsed['is_finite'],
                used_as_finite=parsed['used_as_finite'],
            ),
        )

# CLI arguments
args = CliArguments.parse()
if not args.input_file.exists():
    bad_exit(f"Given file '{args.input_file}' was not found")

asts = parse_file(args.input_file)

egraph = MyEGraph()
for a in asts.values(): egraph.register(a)
for r in all_rules:
    try:
        egraph.register(r)
    except Exception as e:
        e.add_note(f"when registering rule '{r.__name__}'")
        raise
egraph.run(run().saturate())
if args.display: egraph.display()

is_finite = [ int(ast_to_id(ast))
              for ast, in egraph.relation_elements(Ast.is_finite) ]
used_as_finite = [ int(ast_to_id(ast))
                   for ast, in egraph.relation_elements(Ast.used_as_finite) ]
with open(args.output_files.is_finite, 'w+') as f:
    # Wat. `writelines` does not append a '\n' at the end by itself. :|
    # f.writelines(f"{id},{id in is_finite}\n" for id in asts)
    f.writelines(f"{id}\n" for id in is_finite)
with open(args.output_files.used_as_finite, 'w+') as f:
    # f.writelines(f"{id},{id in used_as_finite}\n" for id in asts)
    f.writelines(f"{id}\n" for id in used_as_finite)
