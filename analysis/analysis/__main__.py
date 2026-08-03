from analysis.ast import ast_rules
from analysis.my_egraph import MyEGraph
from analysis.utils import bad_exit
from analysis.input import parse_file
from dataclasses import dataclass
from egglog import run
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
for r in ast_rules: egraph.register(r)
egraph.run(run().saturate())
egraph.display()
