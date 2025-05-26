//! This file is all about printing formulas in latex.

use crate::types::*;
use crate::precedence::Precedence;

// TODO: A `PrettyPrint`/`Format` trait

/// Higher precedence means tighter binding. So a higher number means less need
/// for parentheses.
fn precedence(prop: &Prop) -> Precedence {
    use Precedence::*;
    match prop {
        Prop::Var(..) => Var,
        Prop::Implies(..) => Implies,
        Prop::Forall(..) => Quantifier,
    }
}

/// Formats the given proposition, and adds parenthesis if it is has a
/// precedence lower than the given one.
fn format_prop_prec(prop: &Prop, prec: Precedence) -> String {
    fn needs_parenthesis(prop: &Prop, prec: Precedence) -> bool {
        precedence(prop) < prec
    }

    if needs_parenthesis(prop, prec) {
        format!("({})", format_prop(prop))
    } else {
        format_prop(prop)
    }
}

pub fn format_prop(prop: &Prop) -> String {
    use Prop::*;
    match prop {
        Var(name) => name.clone(),
        Implies(left, right) => {
            format!(
                "{} \\implies {}",
                format_prop_prec(left, Precedence::Implies.inc()),
                format_prop_prec(right, Precedence::Implies.inc()),
            )
        }
        Forall(name, prop) => format!(
            "\\forall {name} {}",
            format_prop_prec(prop, Precedence::Quantifier)
        ),
    }
}

pub fn statement(statement: &Statement) -> String {
    use Statement::*;
    match statement {
        Assume(prop) => format!("Assume {}", format_prop(prop)),
        Assert(prop) => format!("Assert {}", format_prop(prop)),
    }
}

pub fn program(program: &Program) -> String {
    program
        .statements
        .iter()
        .map(statement)
        .collect::<Vec<_>>()
        .join("\n")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t() {
        use Prop::*;
        let prop = Forall(
            "x".to_string(),
            Forall(
                "y".to_string(),
                Implies(
                    Box::new(Var("x".to_string())),
                    Box::new(Implies(
                        Box::new(Var("y".to_string())),
                        Box::new(Var("z".to_string())),
                    )),
                )
                .into(),
            )
            .into(),
        );
        assert_eq!(
            format_prop(&prop),
            "\\forall x \\forall y (x \\implies (y \\implies z))"
        );
    }
}
