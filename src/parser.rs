use crate::ast::expr::*;

use self::{parser::Parsers, token::match_binop_precedence};

pub mod parser;
pub mod token;

type String_ = std::string::String;

impl<'a> Parsers<'a> {
    pub fn new(handler: &mut ExpressionHandler) -> Self {
        Self { handler }
    }
    /// numberexpr ::= number
    pub fn parse_number_expr(&mut self) -> Box<dyn ExprAST> {
        let result = NumberExprAST::new(self.handler.current_value_number);
        self.handler.next_token();
        Box::new(result)
    }

    /// parenexpr ::= '(' expression ')'
    pub fn parse_paren_expr(&mut self) -> Box<dyn ExprAST> {
        self.handler.next_token();
        let result = self.parse_expression();
        if let ParenR = self.handler.current_type {
            self.handler.next_token();
            result
        } else {
            todo!("Error Handler")
        }
    }

    pub fn parse_identifier_expr(&mut self) -> Box<dyn ExprAST> {
        let name = self.handler.current_value_string.clone();
        self.handler.next_token();
        if matches!(self.handler.current_type, ParenL) {
            // Call Expr
            self.handler.next_token();
            let mut args = Vec::<Box<dyn ExprAST>>::new();
            if matches!(self.handler.current_type, ParenR) {
                Box::new(CallExprAST::new(name, args))
            } else {
                loop {
                    let arg = self.parse_expression();
                    args.push(arg);
                    if matches!(self.handler.current_type, ParenR) {
                        break;
                    }
                    if !matches!(self.handler.current_type, Comma) {
                        todo!("Error Handle")
                    }
                    self.handler.next_token();
                }
                Box::new(CallExprAST::new(name, args))
            }
        } else {
            // Var Expr
            Box::new(VariableExprAST::new(name))
        }
    }

    pub fn parse_primary(&mut self) -> Box<dyn ExprAST> {
        match self.handler.current_type {
            Identifier => self.parse_identifier_expr(),
            Number => self.parse_number_expr(),
            ParenL => self.parse_paren_expr(),
            _ => panic!("Undefined parser"),
        }
    }
    pub fn get_token_precedence(&mut self) -> i16 {
        if let Some(val) = match_binop_precedence(&self.handler.current_type) {
            val
        } else {
            -1
        }
    }
    pub fn parse_bin_op_rhs(&mut self, expr_prec: i16, lhs: Box<dyn ExprAST>) -> Box<dyn ExprAST> {
        let mut lhs_ = lhs.as_ref();
        loop {
            let token_prec = self.get_token_precedence();
            if token_prec < expr_prec {
                return lhs;
            }
            let bin_op = self.handler.current_type;
            self.handler.next_token();
            let mut rhs = self.parse_primary();
            let next_prec = self.get_token_precedence();
            if token_prec < next_prec {
                rhs = self.parse_bin_op_rhs(token_prec + 1, rhs);
            }
            lhs_ = &mut BinaryExprAST::new(bin_op, lhs, rhs)
        }
    }

    pub fn parse_prototype(&mut self) {
        if let Identifier = self.handler.current_type {
            let name = self.handler.current_value_string;
            self.handler.next_token();
            if matches!(self.handler.next_token(), ParenL) {
                let mut arg_names = Vec::<String_>::new();
                while matches!(self.handler.next_token(), Identifier) {
                    arg_names.push(self.handler.current_value_string);
                }
                if !matches!(self.handler.next_token(), ParenR) {
                    panic!("Expected ')' in prototype")
                }
            } else {
                panic!("Expected '(' in prototype")
            }
        } else {
            panic!("Expected function name in prototype")
        }
    }

    pub fn parse_expression(&mut self) -> Box<dyn ExprAST> {
        let lhs = self.parse_primary();
        self.parse_bin_op_rhs(0, lhs)
    }
}
