use std::{cell::RefCell, rc::Rc};

use self::{
    parser::Parsers,
    token::{match_binop_precedence, TokenTypes},
};
use crate::compiler::*;
use crate::{ast::expr::*, compiler::codegen::Codegen};

pub mod parser;
pub mod token;

macro_rules! HANDLER {
    ($self:expr) => {
        $self
            .compiler_context
            .as_ref()
            .unwrap()
            .clone()
            .borrow_mut()
            .expression_handler
    };
}
impl<'a> Parsers<'a> {
    /// numberexpr ::= number
    pub fn parse_number_expr(&mut self) -> Box<dyn ExprAST> {
        let result = NumberExprAST::new(HANDLER!(self).current_value_number);
        HANDLER!(self).next_token();
        Box::new(result)
    }

    /// parenexpr ::= '(' expression ')'
    pub fn parse_paren_expr(&mut self) -> Box<dyn ExprAST> {
        HANDLER!(self).next_token();
        let result = self.parse_expression();
        if matches!(&HANDLER!(self).current_type, TokenTypes::ParenR) {
            HANDLER!(self).next_token();
            result
        } else {
            todo!("Error Handler")
        }
    }

    pub fn parse_identifier_expr(&mut self) -> Box<dyn ExprAST> {
        let name = HANDLER!(self).current_value_string.clone();
        HANDLER!(self).next_token();
        if matches!(&HANDLER!(self).current_type, TokenTypes::ParenL) {
            // Call Expr
            HANDLER!(self).next_token();
            let mut args = Vec::<Box<dyn ExprAST>>::new();
            if matches!(&HANDLER!(self).current_type, TokenTypes::ParenR) {
                Box::new(CallExprAST::new(name, args))
            } else {
                loop {
                    let arg = self.parse_expression();
                    args.push(arg);
                    if matches!(&HANDLER!(self).current_type, TokenTypes::ParenR) {
                        break;
                    }
                    if !matches!(&HANDLER!(self).current_type, TokenTypes::Comma) {
                        todo!("Error Handle")
                    }
                    HANDLER!(self).next_token();
                }
                Box::new(CallExprAST::new(name, args))
            }
        } else {
            // Var Expr
            Box::new(VariableExprAST::new(name))
        }
    }

    pub fn parse_primary(&mut self) -> Box<dyn ExprAST> {
        match &HANDLER!(self).current_type {
            TokenTypes::Identifier => self.parse_identifier_expr(),
            TokenTypes::Number => self.parse_number_expr(),
            TokenTypes::ParenL => self.parse_paren_expr(),
            _ => panic!("Undefined parser"),
        }
    }
    pub fn get_token_precedence(&mut self) -> i16 {
        if let Some(val) = match_binop_precedence(&HANDLER!(self).current_type) {
            val
        } else {
            -1
        }
    }
    pub fn parse_bin_op_rhs(&mut self, expr_prec: i16, lhs: Box<dyn ExprAST>) -> Box<dyn ExprAST> {
        let mut lhs_ = lhs.clone();
        loop {
            let token_prec = self.get_token_precedence();
            if token_prec < expr_prec {
                return lhs;
            }
            let bin_op = HANDLER!(self).current_type.clone();
            HANDLER!(self).next_token();
            let mut rhs = self.parse_primary();
            let next_prec = self.get_token_precedence();
            if token_prec < next_prec {
                rhs = self.parse_bin_op_rhs(token_prec + 1, rhs);
            }
            lhs_ = Box::new(BinaryExprAST::new(bin_op, lhs.clone(), rhs))
        }
    }

    pub fn parse_prototype(&mut self) -> Box<dyn ExprAST> {
        if matches!(HANDLER!(self).current_type, TokenTypes::Identifier) {
            let name = HANDLER!(self).current_value_string.clone();
            HANDLER!(self).next_token();
            if matches!(HANDLER!(self).next_token(), TokenTypes::ParenL) {
                let mut arg_names = Vec::<String>::new();
                while matches!(HANDLER!(self).next_token(), TokenTypes::Identifier) {
                    arg_names.push(HANDLER!(self).current_value_string.clone());
                }
                if !matches!(HANDLER!(self).next_token(), TokenTypes::ParenR) {
                    panic!("Expected ')' in prototype")
                }
                Box::new(PrototypeAST::new(name, arg_names))
            } else {
                panic!("Expected '(' in prototype")
            }
        } else {
            panic!("Expected function name in prototype")
        }
    }

    pub fn parse_top_level_expr(&mut self) -> Box<FunctionAST> {
        Box::new(FunctionAST::new(
            Box::new(PrototypeAST::new(String::from(""), vec![])),
            self.parse_expression(),
        ))
    }

    pub fn parse_expression(&mut self) -> Box<dyn ExprAST> {
        let lhs = self.parse_primary();
        self.parse_bin_op_rhs(0, lhs)
    }
}
