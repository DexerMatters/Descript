use std::{
    cell::{RefCell, RefMut},
    rc::Rc,
};

use crate::{ast::expr::ExpressionHandler, compiler::compiler::CompilerContext};

pub struct Parsers<'a> {
    pub compiler_context: Option<Rc<RefCell<CompilerContext<'a>>>>,
}
