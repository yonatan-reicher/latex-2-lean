use latex_2_lean::prelude::*;

fn get_proof(text: &str) -> Proof {
    let ast = markdown::to_mdast(text, &markdown_parse_options()).expect("Valid markdown");
    let proof = parse(&ast).expect("Valid proof");
    proof
}

fn get_lean_code(text: &str) -> Vec<String> {
    to_lean(get_proof(text));
}

#[test]
fn simple_set_definition() {
    let proof = get_proof(r#"
        $$
        A = \{ 1, 2, 3 \}
        $$
    "#);
    assert_eq!(proof.definitions, vec![Definition {
        name: "A".to_string(),
        term: Term::Set(vec![
            Term::Number("1".to_string()),
            Term::Number("2".to_string()),
            Term::Number("3".to_string()),
        ]),
    }]);
}
