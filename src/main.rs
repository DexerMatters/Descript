use std::{cell::RefCell, os::windows::io::HandleOrInvalid, rc::Rc};

use compiler::compiler::CompilerContext;
use inkwell::context::{self, Context};

use crate::ast::expr::ExpressionHandler;

mod ast;
mod compiler;
mod parser;

fn main() {
    let ctx = Context::create();
    let mut context = Rc::new(RefCell::new(CompilerContext::create("nice", &ctx)));
}
