use std::{borrow::BorrowMut, vec};

use crate::parser::{parser::Parsers, token::TokenTypes};

use self::expr::*;

use TokenTypes::*;

pub mod db;
pub mod expr;
pub mod value;

impl ExprAST for NumberExprAST {}
impl ExprAST for BinaryExprAST {}
impl ExprAST for VariableExprAST {}
impl ExprAST for CallExprAST {}
impl ExprAST for PrototypeAST {}
impl ExprAST for ErrorAST {}

type String_ = std::string::String;

impl NumberExprAST {
    pub fn new(val: f64) -> Self {
        Self { val }
    }
}
impl VariableExprAST {
    pub fn new(name: String_) -> Self {
        Self { name }
    }
}
impl BinaryExprAST {
    pub fn new(typ: TokenTypes, lhs: Box<dyn ExprAST>, rhs: Box<dyn ExprAST>) -> Self {
        Self { typ, lhs, rhs }
    }
}
impl CallExprAST {
    pub fn new(name: String_, args: Vec<Box<dyn ExprAST>>) -> Self {
        Self { name, args }
    }
}
impl PrototypeAST {
    pub fn new(name: String_, args: Vec<String_>) -> Self {
        Self { name, args }
    }
}
impl FunctionAST {
    pub fn new(proto: Box<PrototypeAST>, body: Box<dyn ExprAST>) -> Self {
        Self { proto, body }
    }
}

impl<'a> ExpressionHandler<'a> {
    pub fn new(raw: &'a str) -> Self {
        Self {
            raw_iter: raw.chars().into_iter().peekable(),
            current_pos: (0, 0),
            current_value_number: 0f64,
            current_value_string: String_::new(),
            current_type: Undefined,
        }
    }
    fn token_matches(&mut self, x: String_) -> TokenTypes {
        match &x as &str {
            "func" => Function,
            "flow" => Flow,
            "map" => Map,
            "if" => If,
            "else" => Else,
            "for" => For,
            "loop" => Loop,
            "while" => While,
            "break" => Break,
            _ => {
                self.current_value_string = x;
                Identifier
            }
        }
    }

    pub fn next_token(&mut self) -> TokenTypes {
        macro_rules! match_bi {
            ($res0: expr, $({$val:expr ,$res:expr}),*) => {{
                println!("{:?}", self.raw_iter.peek());
                $(
                if self.raw_iter.peek().is_some_and(|x| *x == $val) {
                    self.raw_iter.next();
                    Some($res)
                } else )*
                {
                    Some($res0)
                }
            }}
        }

        let token = match self.raw_iter.next().unwrap() {
            c @ (' ' | '\n' | '\t' | '\r') => {
                if matches!(c, '\n' | '\t') {
                    self.current_pos.0 += 1;
                }
                None
            }

            //check annotations
            '-' => {
                if self.raw_iter.peek().is_some_and(|x| *x == '-') {
                    while self.raw_iter.next().is_some_and(|x| x != '\n') {}
                    None
                } else {
                    Some(Undefined)
                }
            }

            // check symbols
            '=' => match_bi!(As, {'=', Equal}, {'>', Arrow}),

            '+' => match_bi!(Add, {'=', Inc}),
            '-' => match_bi!(Add, {'=', Dec}),
            '*' => match_bi!(Mul, {'=', MulAs}),
            '/' => match_bi!(Div, {'=', DivAs}),
            '%' => match_bi!(Mod, {'=', ModAs}),

            '(' => Some(ParenL),
            ')' => Some(ParenR),
            '[' => Some(BracketL),
            ']' => Some(BracketR),
            '{' => Some(BraceL),
            '}' => Some(BraceR),

            '.' => Some(Dot),
            ',' => Some(Comma),
            ':' => Some(Colon),
            ';' => Some(Semicolon),

            '<' => match_bi!(Less, {'=', LoE}, {'<', ShiftLAs}),
            '>' => match_bi!(Greater, {'=', GoE}, {'>', ShiftRAs}),

            '&' => match_bi!(BinaryAnd, {'&', And}, {'=', AndAs}),
            '|' => match_bi!(BinaryOr, {'|', Or}, {'=', OrAs}),
            '^' => match_bi!(BinaryXor, {'=', XorAs}),
            '!' => match_bi!(Not, {'=', Inequal}),

            // check keywords and ids
            c @ ('a'..='z' | 'A'..='Z') => {
                let mut value = String_::from(c);

                let mut now_char: Option<char> = None;
                while {
                    now_char = self.raw_iter.peek().copied();
                    now_char.is_some_and(|x| x.is_alphanumeric())
                } {
                    self.raw_iter.next();
                    value.push(now_char.unwrap())
                }
                Some(self.token_matches(value))
            }

            // check numbers
            c if {
                matches!(c, '0'..='9' | '.' | '-' | '+')
                    && self
                        .raw_iter
                        .peek()
                        .is_some_and(|x| x.is_numeric() || *x == '.')
            } =>
            {
                let mut value = String_::from(c);
                let mut now_char: Option<char> = None;
                while {
                    now_char = self.raw_iter.peek().copied();
                    now_char.is_some_and(|x| x.is_numeric() || x == '.')
                } {
                    self.raw_iter.next();
                    value.push(now_char.unwrap())
                }
                Some(if let Ok(num) = value.parse::<f64>() {
                    self.current_value_number = num;
                    Number
                } else {
                    Undefined
                })
            }

            // check strings
            _ => Some(Undefined),
        };
        if let Some(token_type) = token {
            if let Undefined = token_type {
                panic!("纯纯的脑瘫语法错误")
            } else {
                self.current_type = token_type.clone();
                token_type
            }
        } else if self.raw_iter.next() == None {
            self.current_type = End;
            End
        } else {
            self.next_token()
        }
    }

    pub fn analyze(&self) {}
}
