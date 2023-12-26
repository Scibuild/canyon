use plex::lexer;

#[derive(Debug)]
pub enum Token {
    Ident(String),
    Int(String),
    True,
    False,
    If,
    While,
    For,
    In,
    Else,
    Import,
    Let,
    Fn,
    Type,
    Return,

    Record,

    Colon,
    Semi,

    Oparen,
    Cparen,
    Obrace,
    Cbrace,
    Obrack,
    Cbrack,

    Add,
    Mul,
    Div,
    Sub,

    Gt,
    Lt,
    Ge,
    Le,
    Eqq,
    Ne,

    Or,
    And,
    Not,

    Dot,
    Comma,
    Eq,

    Comment,
    Whitespace
}

lexer! {
    fn next_token(text: 'a) -> Token;


    "true" => Token::True,
    "false" => Token::False,
    "if" => Token::If,
    "while" => Token::While,
    "for" => Token::For,
    "in" => Token::In,
    "else" => Token::Else,
    "import" => Token::Import,
    "let" => Token::Let,
    "fn" => Token::Fn,
    "type" => Token::Type,
    "return" => Token::Return,
    "record" => Token::Record,
    ":" => Token::Colon,
    ";" => Token::Semi,
    r#"\("# => Token::Oparen,
    r#"\)"# => Token::Cparen,
    "{" => Token::Obrace,
    "}" => Token::Cbrace,
    r#"\["# => Token::Obrack,
    r#"\]"# => Token::Cbrack,
    r#"\+"# => Token::Add,
    r#"\*"# => Token::Mul,
    "/" => Token::Div,
    "-" => Token::Sub,
    ">" => Token::Gt,
    "<" => Token::Lt,
    ">=" => Token::Ge,
    "<=" => Token::Le,
    "==" => Token::Eqq,
    "!=" => Token::Ne,
    "or" => Token::Or,
    "and" => Token::And,
    "!" => Token::Not,
    r#"\."# => Token::Dot,
    "," => Token::Comma,
    "=" => Token::Eq,

    "[a-zA-Z_][a-zA-Z0-9_]*" => Token::Ident(String::from(text)),
    "[0-9]+" => Token::Int(String::from(text)),

    r#"--[^\n]*"# => Token::Comment,
    r#"[ \t\r\n]+"# => Token::Whitespace,

}

pub struct Lexer<'a> {
    original: &'a str,
    remaining: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(s: &'a str) -> Lexer<'a> {
        Lexer { original: s, remaining: s }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub lo: usize,
    pub hi: usize,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = (Token, Span);
    fn next(&mut self) -> Option<(Token, Span)> { 
        loop {
            let (tok, span) = if let Some((tok, new_remaining)) = next_token(self.remaining) {
                let lo = self.original.len() - self.remaining.len();
                let hi = self.original.len() - new_remaining.len();
                self.remaining = new_remaining;
                (tok, Span { lo, hi })
            } else { 
                return None; 
            };

            match tok {
                Token::Whitespace | Token::Comment => {
                    continue;
                }
                tok => {
                    return Some((tok, span));
                }
            }
        }
    }
}
