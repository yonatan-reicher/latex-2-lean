use thiserror::Error;

/** A fact or question that can be proved or disproved. */
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Prop {
    Var(String),
    Implies(Box<Prop>, Box<Prop>),
    Forall(String, Box<Prop>),
}

/** Something that does something to the environment. */
#[derive(Clone, Debug)]
pub enum Statement {
    Assume(Prop),
    Assert(Prop),
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Default)]
pub struct Env {
    pub remembered: Vec<Prop>,
}

// TODO: Move to the verification/checking/semantics module.
#[derive(Debug, Error)]
pub enum Error {
    #[error("Bad assertion")]
    BadAssert,
}
