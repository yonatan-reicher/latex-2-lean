//! This module is responsible for converting definitions to Lean code.

use crate::types::{Definition, Proof, SetComprehension, Term};

pub fn term(t: &Term) -> String {
    match t {
        Term::Number(n) => n.clone(),
        Term::Var(name) => name.clone(),
        Term::Set(terms) => {
            let terms_str: Vec<String> = terms.iter().map(term).collect();
            format!("{{{}}}", terms_str.join(", "))
        }
        Term::SetComprehension(SetComprehension::Filtering {
            variable,
            set,
            condition,
        }) => {
            format!("{{ {} : {} | {} }}", variable, term(set), term(condition),)
        }
        Term::SetComprehension(SetComprehension::Mapping {
            expression,
            condition,
        }) => {
            format!("{{ {} | {} }}", term(expression), term(condition),)
        }
    }
}

pub fn definition(def: &Definition) -> String {
    format!("def {} := {}", def.name, term(&def.term))
}

pub fn proof(proof: &Proof) -> String {
    let definitions_str: Vec<String> = proof.definitions.iter().map(definition).collect();
    definitions_str.join("\n")
}
