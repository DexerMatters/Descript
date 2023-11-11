use std::{iter::Peekable, str::Chars};

use dyn_clone::{clone_trait_object, DynClone};

use crate::parser::token::TokenTypes;

pub trait ExprAST: DynClone {}
clone_trait_object!(ExprAST);

#[derive(Clone)]
pub struct NumberExprAST {
    pub val: f64,
}

#[derive(Clone)]
pub struct VariableExprAST {
    pub name: String,
}

#[derive(Clone)]
pub struct BinaryExprAST {
    pub typ: TokenTypes,
    pub lhs: Box<dyn ExprAST>,
    pub rhs: Box<dyn ExprAST>,
}

#[derive(Clone)]
pub struct CallExprAST {
    pub name: String,
    pub args: Vec<Box<dyn ExprAST>>,
}

#[derive(Clone)]
pub struct PrototypeAST {
    pub name: String,
    pub args: Vec<String>,
}

#[derive(Clone)]
pub struct FunctionAST {
    pub proto: Box<PrototypeAST>,
    pub body: Box<dyn ExprAST>,
}

#[derive(Clone)]
pub struct ErrorAST;

#[derive(Clone)]
pub struct ExpressionHandler<'a> {
    pub raw_iter: Peekable<Chars<'a>>,
    pub current_pos: (usize, usize),
    pub current_type: TokenTypes,
    pub current_value_number: f64,
    pub current_value_string: String,
}
