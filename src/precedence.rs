use unit_enum::UnitEnum;

/// A precedence level for a formula. This is used to determine whether
/// parentheses are needed. This type has an ordering, the higher a precedence
/// on the ordering, the less need for parentheses. So atomic variables have
/// the highest precedence, and quantifiers have very low precedence.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, UnitEnum)]
#[repr(u8)]
pub enum Precedence {
    Implies,
    Quantifier,
    Var,
}

impl Precedence {
    /// Adds, saturating.
    #[inline]
    fn add_i8(self, n: i8) -> Self {
        let d = self.discriminant() as i8;
        let d = (d + n).max(0).min(Self::len() as i8 - 1);
        Self::from_discriminant(d as u8).unwrap()
    }

    #[inline]
    pub fn inc(self) -> Self {
        self.add_i8(1)
    }

    #[inline]
    pub fn dec(self) -> Self {
        self.add_i8(-1)
    }

    #[inline]
    pub fn min() -> Self {
        Self::from_discriminant(0).unwrap()
    }

    #[inline]
    pub fn max() -> Self {
        Self::from_discriminant(Self::len() as u8 - 1).unwrap()
    }
}

