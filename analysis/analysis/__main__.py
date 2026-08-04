from analysis.ast import Ast, AstId, ast_to_id
from analysis.input import parse_file
from analysis.my_egraph import MyEGraph
from analysis.rules import all as all_rules
from analysis.utils import bad_exit
from dataclasses import dataclass
from egglog import Unit, function, run
from pathlib import Path
from sys import argv

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
print('id,is finite,used as finite')
for id in asts:
    print(f"{id},{id in is_finite},{id in used_as_finite}")
