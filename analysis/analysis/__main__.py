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

USAGE_MSG_TEMPLATE = """
Usage: python -m analysis <input_file> MAGIC
""".strip()
USAGE_MSG = USAGE_MSG_TEMPLATE \
    .replace('MAGIC', ' '.join(f"{a}=<path>" for a in analyses))

@dataclass(frozen=True, slots=True)
class OutputFiles:
    is_finite: Path
    used_as_finite: Path

@dataclass(frozen=True, slots=True)
class CliArguments:
    input_file: Path
    output_files: OutputFiles
    @staticmethod
    def parse() -> CliArguments:
        """ Constructs the object from `sys.argv` """
        n_expected_args = 1 + len(analyses)
        if len(argv) != 1 + n_expected_args: bad_exit(USAGE_MSG)
        parsed = {}
        for arg in argv[2:]:
            splot = arg.split('=', maxsplit=1)
            if len(splot) != 2: bad_exit(USAGE_MSG)
            name, path = splot
            parsed[name] = Path(path)
        if frozenset(parsed.keys()) != analyses: bad_exit(USAGE_MSG)
        return CliArguments(
            input_file=Path(argv[1]),
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
for r in all_rules: egraph.register(r)
egraph.run(run().saturate())
# egraph.display()

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
