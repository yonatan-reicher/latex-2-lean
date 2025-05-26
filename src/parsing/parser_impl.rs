use super::parser_trait::Parser as ParserTrait;
use super::parser_trait::Pos;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Bracket {
    Round,
    Square,
    Curly,
    EscapedCurly,
}

impl Bracket {
    pub const fn opening_str(&self) -> &'static str {
        use Bracket::*;
        match self {
            Round => "(",
            Square => "[",
            Curly => "{",
            EscapedCurly => "\\{",
        }
    }

    pub const fn closing_str(&self) -> &'static str {
        use Bracket::*;
        match self {
            Round => ")",
            Square => "]",
            Curly => "}",
            EscapedCurly => "\\}",
        }
    }

    // TODO: Generate this automatically.
    pub const fn all() -> &'static [Bracket] {
        use Bracket::*;
        &[Round, Square, Curly, EscapedCurly]
    }
}

pub struct Parser<'input, E> {
    input: &'input str,
    position: usize,
    error: std::marker::PhantomData<E>,
}

impl<'input, E> Parser<'input, E> {
    pub const fn new(input: &'input str) -> Self {
        Self {
            input,
            position: 0,
            error: std::marker::PhantomData,
        }
    }

    pub fn rest(&self) -> &'input str {
        &self.input[self.position..]
    }

    pub fn skip_whitespace(&mut self) {
        let remaining = &self.input[self.position..];
        let trimmed = remaining.trim_start();
        let to_skip = remaining.len() - trimmed.len();
        self.position += to_skip;
    }
}

impl<'input, E> ParserTrait<'input> for Parser<'input, E> {
    type Err = E;
    type Out<T> = Result<T, Self::Err>;

    fn error<T>(&self, error: Self::Err) -> Self::Out<T> {
        Err(error)
    }

    fn ret<T>(&self, value: T) -> Self::Out<T> {
        Ok(value)
    }

    fn map<T, U>(this: Self::Out<T>, f: impl FnOnce(T) -> U) -> Self::Out<U> {
        this.map(f)
    }

    fn and_then<T, U>(this: Self::Out<T>, f: impl FnOnce(T) -> Self::Out<U>) -> Self::Out<U> {
        this.and_then(f)
    }

    fn pop_keyword(&mut self, keyword: &str) -> bool {
        self.skip_whitespace();
        let remaining = &self.input[self.position..];
        if remaining.starts_with(keyword) {
            // TODO: What if we are matching \forall but the input is \forallx?
            self.position += keyword.len();
            true
        } else {
            false
        }
    }

    fn pop_symbol(&mut self, symbol: &str) -> bool {
        self.skip_whitespace();
        let remaining = &self.input[self.position..];
        if remaining.starts_with(symbol) {
            self.position += symbol.len();
            true
        } else {
            false
        }
    }

    fn pop_name(&mut self) -> Option<&'input str> {
        self.skip_whitespace();
        let remaining = &self.input[self.position..];
        let first_non_letter = remaining
            .char_indices()
            // TODO: Should probably change this condition to allow more characters like ^ but
            // whatever.
            .find(|(_, c)| !c.is_alphanumeric() && *c != '_');
        if let Some((index, _)) = first_non_letter {
            let name = &remaining[..index];
            self.position += index;
            Some(name)
        } else if !remaining.is_empty() {
            let name = remaining;
            self.position += remaining.len();
            Some(name)
        } else {
            None
        }
    }

    type ClosingBracket = Bracket;

    fn pop_bracket(&mut self) -> Option<Self::ClosingBracket> {
        self.skip_whitespace();
        let remaining = &self.input[self.position..];
        let bracket = Bracket::all()
            .iter()
            .cloned()
            .find(|b| remaining.starts_with(b.opening_str()))?;
        self.position += bracket.opening_str().len();
        Some(bracket)
    }

    fn pop_closing_bracket(&mut self, b: Self::ClosingBracket) -> bool {
        self.skip_whitespace();
        let remaining = &self.input[self.position..];
        if remaining.starts_with(b.closing_str()) {
            self.position += b.closing_str().len();
            true
        } else {
            false
        }
    }

    fn eof(&mut self) -> bool {
        self.skip_whitespace();
        self.rest().is_empty()
    }

    fn new(text: &'input str, _start: Pos) -> Self {
        Self::new(text)
    }
}
