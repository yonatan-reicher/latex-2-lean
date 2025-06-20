#[allow(dead_code)]
pub enum Term {
    /// x
    Var(String),
    /// 123
    Number(String),
    /// { 1, 2 }
    Set(Vec<Term>),
    /// { 1, 2 }.toFinset
    Finset(Vec<Term>),
    // { x ∈ N | x > 10 }.toFinset
}

#[allow(dead_code)]
pub enum TopLevel {
    Def {
        name: String,
        term: Term,
    },
}

#[allow(dead_code)]
pub struct Program {
    pub top_levels: Vec<TopLevel>,
}
