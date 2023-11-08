use crate::ast::expr::ExpressionHandler;

mod ast;
mod parser;

fn main() {
    let mut handle = ExpressionHandler::new("222");
}
