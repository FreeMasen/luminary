use analisar::aware::ast::{
    Args, Block, ExpListItem, Expression, Field, FuncBody, FunctionCall, ParListPart, Statement,
    Table,
};
use lex_lua::Span;
use core::panic;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

static NIL_EXPR: &Expression<'static> = &Expression::Nil(Span { start: 0, end: 0 });

#[derive(Debug, Default, Clone)]
pub struct SymbolTable {
    t: HashMap<String, Rc<RefCell<SymbolTableEntry>>>,
}

impl SymbolTable {
    pub fn insert(&mut self, k: String, entry: impl Into<Rc<RefCell<SymbolTableEntry>>>) {
        self.t.insert(k, entry.into());
    }
}

#[derive(Debug, Clone)]
pub struct SymbolTableEntry {
    symbol: String,
    parent: Rc<RefCell<SymbolTable>>,
    value: SymbolValue,
}

impl Into<Rc<RefCell<Self>>> for SymbolTableEntry {
    fn into(self) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(self))
    }
}

#[derive(Debug, Clone)]
pub enum SymbolValue {
    NilLit,
    BoolLit(bool),
    NumberLit(f32),
    StringLit(Vec<u8>),
    TableLit(Rc<RefCell<SymbolTable>>),
    FunctionDecl {
        args: Rc<RefCell<SymbolTable>>,
        scope: Rc<RefCell<SymbolTable>>,
    },
    Symbol(Rc<RefCell<SymbolTableEntry>>),
    None,
}

impl SymbolTableEntry {
    pub fn new(symbol: String, parent: Rc<RefCell<SymbolTable>>, value: SymbolValue) -> Self {
        Self {
            symbol,
            parent,
            value,
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct SymbolTableVisitor {
    pub global: Rc<RefCell<SymbolTable>>,
    pub module: Rc<RefCell<SymbolTable>>,
    pub scopes: Vec<Rc<RefCell<SymbolTable>>>,
}

impl SymbolTableVisitor {
    pub fn visit_stmt(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Empty(_) => {}
            Statement::Expression(expr) => self.visit_expr_stmt(expr),
            Statement::Assignment {
                local_span,
                targets,
                eq_span: _,
                values,
            } => self.visit_assignment(local_span.as_ref(), targets, values),
            Statement::Label {
                colons1_span,
                name,
                colons2_span,
            } => todo!(),
            Statement::Break(_) => todo!(),
            Statement::GoTo { goto_span, label } => todo!(),
            Statement::Do {
                do_span,
                block,
                end_span,
            } => todo!(),
            Statement::While {
                while_span,
                exp,
                do_span,
                block,
                end_span,
            } => todo!(),
            Statement::Repeat {
                repeat_span,
                block,
                until_span,
                exp,
            } => todo!(),
            Statement::If(_) => todo!(),
            Statement::For(_) => todo!(),
            Statement::ForIn(_) => todo!(),
            Statement::Function {
                local,
                function,
                name,
                body,
            } => todo!(),
            Statement::Return(_) => todo!(),
        }
    }

    fn visit_expr_stmt(&mut self, expr: &Expression) {
        match expr {
            Expression::Nil(_) => todo!(),
            Expression::False(_) => todo!(),
            Expression::True(_) => todo!(),
            Expression::Numeral(_) => todo!(),
            Expression::LiteralString(_) => todo!(),
            Expression::Name(_) => todo!(),
            Expression::VarArgs(_) => todo!(),
            Expression::FunctionDef(_) => todo!(),
            Expression::TableCtor(_) => todo!(),
            Expression::Parened {
                open_span,
                expr,
                close_span,
            } => todo!(),
            Expression::BinOp { left, op, right } => todo!(),
            Expression::UnaryOp { op, exp } => todo!(),
            Expression::FuncCall(_) => todo!(),
            Expression::Suffixed(_) => todo!(),
        }
    }

    fn visit_assignment(
        &mut self,
        local_span: Option<&Span>,
        targets: &[ExpListItem],
        values: &[ExpListItem],
    ) {
        for (idx, target) in targets.iter().enumerate() {
            let ExpListItem::Expr(target) = target else {
                // skip the commas
                continue;
            };
            let value = values
                .get(idx)
                .into_iter()
                .filter_map(|li| match li {
                    ExpListItem::Comma(_) => None,
                    ExpListItem::Expr(expr) => Some(expr),
                })
                .next()
                .unwrap_or_else(|| NIL_EXPR);

            let value = match value {
                Expression::Nil(_) => SymbolValue::NilLit,
                Expression::False(_) => SymbolValue::BoolLit(false),
                Expression::True(_) => SymbolValue::BoolLit(true),
                Expression::Numeral(n) => {
                    let v = n.numeral.parse::<f32>().unwrap();
                    SymbolValue::NumberLit(v)
                }
                Expression::LiteralString(s) => SymbolValue::StringLit(s.value.to_vec()),
                Expression::Name(n) => {
                    if let Some(global) = self.global.borrow().t.get(&*n.name) {
                        SymbolValue::Symbol(global.clone())
                    } else if let Some(module) = self.module.borrow().t.get(&*n.name) {
                        SymbolValue::Symbol(module.clone())
                    } else {
                        SymbolValue::None
                    }
                }
                Expression::VarArgs(_) => {
                    todo!()
                }
                Expression::FunctionDef(def) => {
                    let args = Rc::new(RefCell::new(SymbolTable::default()));
                    let scope = Rc::new(RefCell::new(SymbolTable::default()));
                    for v in &def.par_list.parts {
                        match v {
                            ParListPart::Comma(_) => continue,
                            ParListPart::Name(v) => {
                                let value = if let Some(global) =
                                    self.global.borrow().t.get(&*v.name)
                                {
                                    SymbolValue::Symbol(global.clone())
                                } else if let Some(module) = self.module.borrow().t.get(&*v.name) {
                                    SymbolValue::Symbol(module.clone())
                                } else {
                                    SymbolValue::None
                                };
                                let entry = SymbolTableEntry {
                                    symbol: v.name.to_string(),
                                    parent: args.clone(),
                                    value,
                                };
                                args.borrow_mut()
                                    .insert(v.name.to_string(), entry);
                            }
                            ParListPart::VarArgs(_v) => {
                                let entry = SymbolTableEntry {
                                    symbol: "...".to_string(),
                                    value: SymbolValue::None,
                                    parent: args.clone(),
                                };
                                args.borrow_mut()
                                    .insert("...".to_string(), entry);
                            }
                        }
                    }
                    SymbolValue::FunctionDecl { args, scope }
                }
                Expression::TableCtor(table_lit) => {
                    let t = Rc::new(RefCell::new(SymbolTable::default()));

                    todo!()
                }
                Expression::Parened { expr, .. } => {
                    todo!()
                }
                Expression::BinOp { left, op, right } => {
                    todo!()
                }
                Expression::UnaryOp { op, exp } => {
                    todo!()
                }
                Expression::FuncCall(_) => {
                    todo!()
                }
                Expression::Suffixed(_) => {
                    todo!()
                }
            };
            match target {
                Expression::Name(name) => {
                    if local_span.is_some() {
                        self.global.borrow_mut().t.insert(
                            name.name.to_string(),
                            Rc::new(RefCell::new(SymbolTableEntry {
                                symbol: name.name.to_string(),
                                parent: self.global.clone(),
                                value,
                            })),
                        );
                    } else {
                        self.module.borrow_mut().t.insert(
                            name.name.to_string(),
                            Rc::new(RefCell::new(SymbolTableEntry {
                                symbol: name.name.to_string(),
                                parent: self.module.clone(),
                                value,
                            })),
                        );
                    }
                }
                Expression::Suffixed(expr) => {
                    todo!("Assigning to suffixed");
                }
                _ => panic!("assigning to non-name or suffixed expression"),
            }
        }
    }
}


#[cfg(test)]
mod test {
    use super::*;
    
    #[test]
    fn build_symbol_table_happy() {
        let lua = r#"a = 1
b = 2.5
d = "hello world"
e = true
f = false
"#;
        let visitor = SymbolTableVisitor::default();

    }
}
