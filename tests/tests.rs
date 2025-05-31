use latex_2_lean::*;
use indoc::indoc;

fn get_proof(text: &str) -> Proof {
    let ast = markdown::to_mdast(text, &markdown_parse_options()).expect("Valid markdown");
    parse(&ast).expect("Valid proof")
}

fn get_lean_code(text: &str) -> String {
    lean_to_string(&ast_to_lean(&get_proof(text)))
}

#[test]
fn simple_set_definition() {
    let proof = get_proof(indoc! {r#"
        $$
        A = \{ 1, 2, 3 \}
        $$
    "#});
    assert_eq!(proof.definitions, vec![Definition {
        name: "A".to_string(),
        term: Term::Set(vec![
            Term::Number("1".to_string()),
            Term::Number("2".to_string()),
            Term::Number("3".to_string()),
        ]),
    }]);
}

#[test]
fn simple_lean_code() {
    let lean = get_lean_code(indoc! {r#"
        $$
        A = \{ 1, 2, 3 \}
        $$
    "#});
    assert_eq!(lean, indoc! {r#"
        def A := { 1, 2, 3 }
    "#});
}
