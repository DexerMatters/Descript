use crate::ast::expr::ExpressionHandler;

pub struct Parsers<'a> {
    pub handler: &'a mut ExpressionHandler<'a>,
}
