"""
This file is for preprocessing our soufflé code. This is for making our code
less needlessly verbose!

This file just searches for syntax terms of the form:
```
    `<name>(<arg-1>, <arg-2>, ...)`
```
and replaces them with:
```souffle
    [ <name>, [ <arg-1>, [ <arg-2>, ... ] ] ]
```
(Matching the definition for expressions in `types.dl`)
"""

from sys import argv
from pathlib import Path
from typing import Literal


def error(msg: str):
    print(f"Error: {msg}")
    exit(1)


def find_any(text: str, substrings: list[str]) -> int | None:
    try:
        return min(i for s in substrings if (i := text.find(s)) != -1)
    except:
        return None


def find_but_not_in_paren(
    text: str,
    sub: str,
    start: int = 0,
) -> int | Literal['unclosed-paren'] | None:
    """
    Finds the given substring, but skips over parenthesis and nested
    parenthesis, and if the substring is found, but only after another closing
    parenthesis that were not open, treats as unfound.

    ## Example
    ```python
    assert find_but_not_in_paren('abc, de(fg), hi, abc) abc, sf', 'abc') == 0
    assert find_but_not_in_paren(', de(fg), hi, abc) abc, sf', 'abc') == 14
    assert find_but_not_in_paren(') abc, sf', 'abc') == None
    assert find_but_not_in_paren('( abc, sf', 'abc') == 'unclosed-paren'
    ```
    """
    assert not sub.startswith('(')
    i = find_any(text[start:], ['(', ')', sub])
    if i is None:
        return None
    i += start  # Adjust index to the original text.
    if text[i:].startswith(sub):
        return i
    if text[i] == ')':
        return None
    if text[i] == '(':
        end_index = find_but_not_in_paren(text, ')', start=i + 1)
        if end_index is None or end_index == 'unclosed-paren':
            return 'unclosed-paren'
        # Now we keep searching, after the parenthesis.
        return find_but_not_in_paren(text, sub, start=end_index + 1)


def parse_coma_seperated_with_paren(
    text: str,
    verbose: bool = False,
) -> tuple[list[str], str]:
    """
    Parses text of the format `(hello world(cool), (beans, cat), meowws) wooo!` into
    `([ 'hello world(cool)', '(beans, cat)', 'meows' ], 'woo!)`
    """
    # Works by repeatedly finding the arguments. Basically a hand-written
    # parser.
    # Another approach I considered was this:
    # https://stackoverflow.com/a/50966148/10045438
    """
    r = r'(?> ( \( (?> [^()]*(?1)?)* \) ) | [^,()]+ | (?P<error>[()]+) )+'
    return [
        m.group().strip()
        for m in re.finditer(text, r)
    ]
    """
    assert len(text) > 0 and text[0] == '('
    rest = text[1:]
    args = []
    while True:
        if verbose:
            print(f"Parsing arguments from rest: {rest}")
        if rest.startswith(')'):
            return args, ''
        after_arg = find_but_not_in_paren(rest, ',')
        if verbose:
            print(f"- Looking for ',': {after_arg=}")
        if after_arg is None:
            after_arg = find_but_not_in_paren(rest, ')')
            if verbose:
                print(f"  Looking for ')': {after_arg=}")
            assert after_arg is not None
        if after_arg == 'unclosed-paren':
            error(
                f"Parenthesis unclosed in the following rest of the line:\n"
                f"{text}"
            )
        else:
            if verbose:
                print(f"  {after_arg=}")
            args.append(rest[:after_arg])
            if rest[after_arg] == ')':
                return args, rest[after_arg+1:]
            rest = rest[after_arg + 1:]


def preprocess_text(
    text: str,
    path: Path | None = None,
    verbose=False,
) -> str:
    if verbose:
        print(f"Processing line: {text.splitlines()[0]}")
    while True:
        start_of_macro = text.find('`')
        if start_of_macro == -1:
            return text
        if verbose:
            print(f"- Found macro!")
        # The rest of the text after the index
        rest = text[start_of_macro+1:]
        if len(rest) == 0:
            error(f"cannot have '`' at the end of a file ({path=})")
        if rest[0] == '(':
            # TODO
            raise NotImplemented(
                'We need to do some special parsing to '
                'skip after the parenthesis '
            )
        else:
            name_end_no_arg_delim = (' ', '\t', '\n', ',', ')')
            name_end = find_any(rest, [*name_end_no_arg_delim, '(']) or len(rest)
            name_term = rest[:name_end]
            if verbose:
                print(f"  Found name term: {name_term}")
            rest = rest[name_end:]
            if rest == '' or rest[0] in name_end_no_arg_delim:
                text = text.replace(
                    '`' + name_term,
                    f"[ {name_term}, nil ]",
                    1,
                )
            else:
                args, rest = parse_coma_seperated_with_paren(rest,
                                                             verbose=verbose)
                if verbose:
                    print(f"  Found args: {args}")
                before = text[:start_of_macro]
                after = rest
                args_str = 'nil'
                args.reverse()
                for a in args:
                    args_str = f"[ {a}, {args_str} ]"
                text = f"{before}[ {name_term}, {args_str} ]{after}"
        if verbose:
            print(f"  Replaced text: {text}")


def backwards_step(text: str) -> str | None:
    if (
        text.startswith('[')
    and isinstance((name_end := find_but_not_in_paren(text, ',')), int)
    and '[' not in (name := text[1:name_end])
    ):
        rest = text[name_end:]
        if rest.startswith(', nil]'):
            rest = rest[len(', nil]'):]
            text = f"`{name}{rest}"
            return text
        if rest.startswith(', `'):
            rest = rest[len(', `'):]
            arg_end = find_but_not_in_paren(rest, ']')
            if not isinstance(arg_end, int):
                return None
            arg = rest[:arg_end]
            rest = rest[arg_end + 1:]
            if '[' in arg:
                return None
            return f"`{name}({arg}){rest}"
        """
        if rest.startswith(', ['):
            rest = rest[len(', ['):]
            arg_end = find_but_not_in_paren(rest, ',')
            if not isinstance(arg_end, int):
                return None
            arg1 = rest[:arg_end]
            rest = rest[arg_end + 1:]
            print(f"arg1: {arg1}")
            if '[' in arg1:
                return None
            if not rest.startswith(' `'):
                return None
            rest = rest[2:]  # Skip the space and backtick.
            arg_end = find_but_not_in_paren(rest, ']')
            if not isinstance(arg_end, int):
                return None
            arg2 = rest[:arg_end]
            rest = rest[arg_end + 1:]
            text = f"`{name}({arg1}, {arg2}){rest}"
            return text
        """


def backwards_step_somewhere(text: str) -> str | None:
    prefix = ''
    while len(text) > 0:
        if (changed := backwards_step(text)) is not None:
            return prefix + changed
        prefix += text[0]
        text = text[1:]


def backwards(text: str) -> str:
    while (next := backwards_step_somewhere(text)) is not None:
        text = next
    return text


def preprocess(src_dir: Path, dest_dir: Path):
    for path in src_dir.rglob('*.dl'):
        assert path.parts[:len(src_dir.parts)] == src_dir.parts
        output_path = dest_dir.joinpath(*path.parts[len(src_dir.parts):])
        text = path.read_text()
        text = preprocess_text(text, path)
        output_path.write_text(text)


def parse_args() -> tuple[Path, Path]:
    if len(argv) != 3:
        print("Usage: python preprocess.py <source-dir> <dest-dir>")
        exit(1)
    src_dir = Path(argv[1])
    if not src_dir.exists():
        error(f"source dir {src_dir} does not exist!")
    return src_dir, Path(argv[2])


def main():
    src_dir, dest_dir = parse_args()
    preprocess(src_dir, dest_dir)


if __name__ == '__main__':
    main()
