"""
To be ran with pytest.
"""

from preprocess import preprocess_text, backwards


def test_backwards():
    out = backwards("""
        [name, nil]
    """)
    assert out.strip() == '`name'


def test_backwards_nested():
    out = backwards("""
        [name, [[bame, nil], nil]]
    """)
    assert out.strip() == '`name(`bame)'


def test_backwards_nested_2_args():
    out = backwards("""
        [name, [[bame, nil], nil]]
    """)
    assert out.strip() == '`name(`bame)'


def test_argless():
    out = preprocess_text("""
        `name
    """, verbose=True)
    assert out.strip(
    ) == '[ name, nil ]'


def test_argless_in_paren():
    out = preprocess_text("""
        (`name)
    """, verbose=True)
    assert out.strip(
    ) == '([ name, nil ])'


def test_empty_args():
    out = preprocess_text("""
        `"new-set"()
    """, verbose=True)
    assert out.strip(
    ) == '[ "new-set", nil ]'


def test_nested_2():
    out = preprocess_text("""
        `"new-set"(`"abs")
    """, verbose=True)
    assert out.strip(
    ) == '[ "new-set", [ [ "abs", nil ], nil ] ]'


def test_another_nested_2():
    out = preprocess_text("""
        `"new-set"(`"abs", `"abs")
    """, verbose=True)
    assert out.strip(
    ) == '[ "new-set", [ [ "abs", nil ], [  [ "abs", nil ], nil ] ] ]'


def test_nested_2_with_arg():
    out = preprocess_text("""
        `"new-set"(`"abs"(A))
    """, verbose=True)
    assert out.strip(
    ) == '[ "new-set", [ [ "abs", [ A, nil ] ], nil ] ]'


def test_nested_3():
    out = preprocess_text("""
        `"new-set"(`"abs"(`"A"))
    """, verbose=True)
    assert out.strip(
    ) == '[ "new-set", [ [ "abs", [ [ "A", nil ], nil ] ], nil ] ]'


def assert_lines_stripped_equal(a, b):
    a = a.strip().splitlines()
    b = b.strip().splitlines()
    assert len(a) == len(b), f"Length mismatch: {len(a)} != {len(b)}"
    for i, (a_line, b_line) in enumerate(zip(a, b)):
        assert a_line.strip() == b_line.strip(
        ), f"Line {i+1} mismatch: '{a_line.strip()}' != '{b_line.strip()}'"


def test_nested_3_with_newlines():
    out = preprocess_text("""
        `"new-set"(
            `"abs"(`"A")
        )
    """, verbose=True)
    print(f"{out=}")
    assert_lines_stripped_equal(out, '''
        [ "new-set", [
            [ "abs", [ [ "A", nil ], nil ] ]
        , nil ] ]
    ''')
