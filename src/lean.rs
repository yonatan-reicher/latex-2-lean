pub enum Term {
    /// x
    Var(String),
    /// 123
    Number(String),
    /// { 1, 2 }
    Set,
    /// { 1, 2 }.toFinset
    Finset,
    // { x ∈ N | x > 10 }.toFinset
}
