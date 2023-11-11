use std::{array::IntoIter, iter::Peekable, str::Chars, vec};

use crate::parser::token::TokenTypes;

pub trait ExprAST {}
pub struct NumberExprAST {
    pub val: f64,
}
pub struct VariableExprAST {
    pub name: String,
}
pub struct BinaryExprAST {
    pub typ: TokenTypes,
    pub lhs: Box<dyn ExprAST>,
    pub rhs: Box<dyn ExprAST>,
}
pub struct CallExprAST {
    pub name: String,
    pub args: Vec<Box<dyn ExprAST>>,
}

pub struct PrototypeAST {
    pub name: String,
    pub args: Vec<String>,
}

pub struct FunctionAST {
    pub proto: Box<PrototypeAST>,
    pub body: Box<dyn ExprAST>,
}

pub struct ErrorAST;

pub struct ExpressionHandler<'a> {
    pub raw_iter: Peekable<Chars<'a>>,
    pub current_pos: (usize, usize),
    pub current_type: TokenTypes,
    pub current_value_number: f64,
    pub current_value_string: String,
}
