use std::{
    cell::{RefCell, RefMut},
    rc::Rc,
    sync::Arc,
};

use inkwell::{
    builder::Builder,
    context::{Context, ContextRef},
    execution_engine::ExecutionEngine,
    module::Module,
};

use super::compiler::CompilerContext;

pub struct Codegen<'ctx> {
    pub context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,
    pub engine: ExecutionEngine<'ctx>,
    pub compiler_context: Option<Rc<RefCell<CompilerContext<'ctx>>>>,
}
