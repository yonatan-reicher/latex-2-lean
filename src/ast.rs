#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SetComprehension {
    /// { x ∈ N | x > 10 }
    Filtering {
        variable: String,
        set: Box<Term>,
        condition: Box<Term>,
    },
    /// { x + 1 | x ∈ N, x > 9 }
    Mapping {
        expression: Box<Term>,
        condition: Box<Term>,
    },
}

/// A latex term such as `x` of `\{ x \in \mathbb{N} \mid x > 10 \}`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Term {
    Var(String),
    Number(String),
    Set(Vec<Term>),
    SetComprehension(SetComprehension),
    // TODO
}

/// A definition of something that we want to convert to lean.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Definition {
    pub name: String,
    pub term: Term,
}

#[derive(Debug)]
pub struct Proof {
    pub definitions: Vec<Definition>,
}
