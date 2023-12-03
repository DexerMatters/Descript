use crate::{ast::expr::ExpressionHandler, parser::parser::Parsers};

use super::codegen::Codegen;

pub struct CompilerContext<'a> {
    pub raw_code: &'a str,
    pub expression_handler: ExpressionHandler<'a>,
    pub codegen: Codegen<'a>,
    pub parsers: Parsers<'a>,
}
