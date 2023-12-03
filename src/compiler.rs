use std::{cell::RefCell, rc::Rc, sync::Arc};

use inkwell::context::{AsContextRef, Context, ContextRef};

use crate::{ast::expr::ExpressionHandler, parser::parser::Parsers};

use self::{codegen::Codegen, compiler::CompilerContext};

pub mod codegen;
pub mod compiler;

impl<'ctx> Codegen<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let module = context.create_module("test");
        let builder = context.create_builder();
        let engine = module.create_execution_engine().unwrap();
        Self {
            context: &context,
            builder,
            module,
            engine,
            compiler_context: None,
        }
    }
}

impl<'a> CompilerContext<'a> {
    pub fn create(raw_code: &'a str, context: &'a Context) -> CompilerContext<'a> {
        Self {
            raw_code,
            expression_handler: ExpressionHandler::new(raw_code),
            codegen: Codegen::new(&context),
            parsers: Parsers {
                compiler_context: None,
            },
        }
    }
    pub fn init_context(context: Rc<RefCell<CompilerContext<'a>>>) {
        let mut context_ref = context.borrow_mut();
        context_ref.expression_handler.compiler_context = Some(context.clone());
        context_ref.codegen.compiler_context = Some(context.clone());
        context_ref.parsers.compiler_context = Some(context.clone());
    }
}
