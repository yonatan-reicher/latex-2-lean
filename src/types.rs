/// A latex term such as `x` of `\{ x \in \mathbb{N} \mid x > 10 \}`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Term {
    Var(String),
    Number(String),
    // TODO
}

/// A definition of something that we want to convert to lean.
#[derive(Clone, Debug)]
pub struct Definition {
    pub name: String,
    pub term: Term,
}

#[derive(Debug)]
pub struct Proof {
    pub definitions: Vec<Definition>,
}
