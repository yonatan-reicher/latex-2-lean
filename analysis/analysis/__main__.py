from analysis.my_egraph import MyEGraph
from analysis.utils import bad_exit
import analysis.node
from dataclasses import dataclass
from pathlib import Path
from sys import argv

@dataclass(frozen=True, slots=True)
class CliArguments:
    input_file: Path
    @staticmethod
    def parse() -> CliArguments:
        if len(argv) != 2:
            bad_exit("Usage: python -m egglog <input_file>")
        return CliArguments(input_file=Path(argv[1]))

args = CliArguments.parse()
if not args.input_file.exists():
    bad_exit(f"Given file '{args.input_file}' was not found")

text = args.input_file.read_text()
lines = text.splitlines()
if lines == []:
    bad_exit(f"The input file '{args.input_file}' was empty - the input file "
        "must at least have a header as it's first line")
header = lines.pop(0)
expected_header = "id,kind,arguments"
if header != expected_header:
    bad_exit(f"The input file '{args.input_file}' had a wrong header - the "
        f"file's header must be:\n\t'{expected_header}'\n"
        f"but instead was:\n\t'{header}'")

print("🦮")
