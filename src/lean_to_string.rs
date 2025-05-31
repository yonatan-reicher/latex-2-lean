//! This module is responsible for converting definitions to Lean code.

use crate::lean::{Program, TopLevel, Term};
use flat_vec::flat_vec;

fn term(t: &Term) -> Vec<String> {
    match t {
        Term::Var(name) => vec![name.to_string()],
        Term::Number(num) => vec![num.to_string()],
        Term::Set => todo!(),
        Term::Finset => todo!(),
    }
}

fn top_level(tl: &TopLevel) -> Vec<String> {
    match tl {
        TopLevel::Def { name, term:t } => {
            flat_vec![
                format!("def {name} :="),
                flat term(t),
            ]
        }
    }
}

fn program(p: &Program) -> Vec<String> {
    p.top_levels
        .iter()
        .flat_map(top_level)
        .collect()
}

pub fn lean_to_string(p: &Program) -> String {
    program(p).join("\n")
}

/*
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

pub fn definition(def: &Definition) -> Vec<String> {
    vec![format!("def {} := {}", def.name, term(&def.term))]
}

pub fn proof(proof: &Proof) -> Vec<String> {
    proof.definitions.iter().flat_map(definition).collect()
}
*/
