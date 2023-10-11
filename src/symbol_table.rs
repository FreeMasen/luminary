use analisar::aware::ast::{
    Args, Block, ExpListItem, Expression, Field, FuncBody, FunctionCall, ParListPart, Statement,
    Table,
};
use indexmap::IndexMap;
use lex_lua::Span;
use std::{arch::global_asm, cell::RefCell, fmt::Debug, rc::Rc};

static NIL_EXPR: &Expression<'static> = &Expression::Nil(Span { start: 0, end: 0 });

#[derive(Default, Clone)]
pub struct SymbolTable {
    parent: Option<Rc<RefCell<Self>>>,
    t: IndexMap<String, Rc<RefCell<SymbolTableEntry>>>,
}

impl Debug for SymbolTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut d = if self.parent.is_none() {
            f.debug_struct("GlobalSymbolTable")
        } else {
            f.debug_struct("SymbolTable")
        };
        for (k, v) in self.t.iter() {
            let entry = v.as_ref().borrow();
            d.field(k, &entry.value);
        }
        d.finish()
    }
}

impl SymbolTable {
    pub fn insert(&mut self, k: String, entry: impl Into<Rc<RefCell<SymbolTableEntry>>>) {
        self.t.insert(k, entry.into());
    }

    pub fn find(&self, k: &str) -> Option<Rc<RefCell<SymbolTableEntry>>> {
        if let Some(v) = self.t.get(k) {
            return Some(v.clone());
        }
        if let Some(parent) = &self.parent {
            return parent.borrow().find(k);
        }
        None
    }
}

#[derive(Clone)]
pub struct SymbolTableEntry {
    symbol: String,
    parent: Rc<RefCell<SymbolTable>>,
    value: SymbolValue,
}

impl Debug for SymbolTableEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SymbolTableEntry")
            .field("symbol", &self.symbol)
            .field("value", &self.value)
            .finish_non_exhaustive()
    }
}

impl Into<Rc<RefCell<Self>>> for SymbolTableEntry {
    fn into(self) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(self))
    }
}

#[derive(Clone)]
pub enum SymbolValue {
    NilLit,
    BoolLit(bool),
    NumberLit(f32),
    StringLit(Vec<u8>),
    TableLit(Rc<RefCell<SymbolTable>>),
    FunctionDecl {
        args: Rc<RefCell<Vec<(String, SymbolValue)>>>,
        scope: Rc<RefCell<SymbolTable>>,
    },
    Symbol(Rc<RefCell<SymbolTableEntry>>),
    Name(String),
    VarArgs,
    Label(String),
    None,
}

impl Debug for SymbolValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NilLit => write!(f, "nil"),
            Self::BoolLit(arg0) => write!(f, "{}", arg0),
            Self::NumberLit(arg0) => write!(f, "{}", arg0),
            Self::StringLit(arg0) => write!(f, "{}", &String::from_utf8_lossy(arg0)),
            Self::TableLit(arg0) => {
                let mut d = f.debug_struct("");
                for (k, v) in arg0.borrow().t.iter() {
                    d.field(k.as_str(), &v.borrow().value);
                }
                d.finish()
            }
            Self::FunctionDecl { args, scope } => {
                struct FunctionArguments<'a>(&'a [(String, SymbolValue)]);
                impl<'a> Debug for FunctionArguments<'a> {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        let mut d = f.debug_struct("");
                        for (k, v) in self.0 {
                            d.field(k, &*v);
                        }
                        d.finish()
                    }
                }
                struct FunctionScope<'a>(&'a SymbolTable);
                impl<'a> Debug for FunctionScope<'a> {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        let mut d = f.debug_struct("");
                        for (k, v) in self.0.t.iter() {
                            d.field(k, &v.borrow().value);
                        }
                        d.finish()
                    }
                }

                let mut d = f.debug_struct("Function");
                d.field("args", &FunctionArguments(&*args.borrow()));
                d.field("scope", &FunctionScope(&*scope.borrow()));
                d.finish()
            }
            Self::Symbol(arg0) => {
                write!(f, "{:?}", arg0.borrow().symbol)
            }
            SymbolValue::VarArgs => f.write_str("..."),
            SymbolValue::Name(n) => f.write_str(n),
            SymbolValue::Label(n) => write!(f, "::{n}::"),
            Self::None => f.write_str("None"),
        }
    }
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

#[derive(Clone)]
pub struct SymbolTableVisitor {
    pub global: Rc<RefCell<SymbolTable>>,
    pub module: Rc<RefCell<SymbolTable>>,
    pub scopes: Vec<Rc<RefCell<SymbolTable>>>,
}

impl Default for SymbolTableVisitor {
    fn default() -> Self {
        let global: Rc<RefCell<SymbolTable>> = Default::default();
        let module: Rc<RefCell<SymbolTable>> = Default::default();
        module.borrow_mut().parent = Some(global.clone());
        Self {
            global,
            module,
            scopes: Vec::new(),
        }
    }
}

impl Debug for SymbolTableVisitor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut d = f.debug_struct("SymbolTableVisitor");
        d.field("global", &*self.global.as_ref().borrow())
            .field("module", &*self.global.as_ref().borrow());
        for (idx, s) in self.scopes.iter().enumerate() {
            d.field(&idx.to_string(), &*s.borrow());
        }
        d.finish()
    }
}

impl SymbolTableVisitor {
    pub fn visit_stmt(&mut self, stmt: &Statement) {
        Self::visit_stmt_continued(self.global.clone(), self.module.clone(), stmt)
    }
    fn visit_stmt_continued(
        global: Rc<RefCell<SymbolTable>>,
        current: Rc<RefCell<SymbolTable>>,
        stmt: &Statement,
    ) {
        match stmt {
            Statement::Empty(_) => {}
            Statement::Expression(expr) => {
                Self::visit_expr_stmt_continued(global.clone(), current.clone(), expr)
            }
            Statement::Assignment {
                local_span,
                targets,
                eq_span: _,
                values,
            } => Self::visit_assignment_continued(
                global.clone(),
                current.clone(),
                local_span.as_ref(),
                targets,
                values,
            ),
            Statement::Label { name, .. } => {
                current.borrow_mut().insert(
                    name.name.to_string(),
                    SymbolTableEntry {
                        parent: current.clone(),
                        symbol: name.name.to_string(),
                        value: SymbolValue::Label(name.name.to_string()),
                    },
                );
            }
            Statement::Break(_) => todo!(),
            Statement::GoTo { goto_span, label } => todo!(),
            Statement::Do {
                do_span,
                block,
                end_span,
            } => {
                let _do_scope = SymbolTable::default();
            }
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
        Self::visit_expr_stmt_continued(self.global.clone(), self.module.clone(), expr)
    }

    fn visit_expr_stmt_continued(
        global: Rc<RefCell<SymbolTable>>,
        current: Rc<RefCell<SymbolTable>>,
        expr: &Expression,
    ) {
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
        Self::visit_assignment_continued(
            self.global.clone(),
            self.module.clone(),
            local_span,
            targets,
            values,
        )
    }
    fn visit_assignment_continued(
        global: Rc<RefCell<SymbolTable>>,
        current: Rc<RefCell<SymbolTable>>,
        local_span: Option<&Span>,
        targets: &[ExpListItem],
        values: &[ExpListItem],
    ) {
        println!("visit_assignment_continued {local_span:?}");
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

            let value = Self::visit_assignment_source(global.clone(), current.clone(), value);
            match target {
                Expression::Name(name) => {
                    if local_span.is_some() {
                        current.borrow_mut().t.insert(
                            name.name.to_string(),
                            Rc::new(RefCell::new(SymbolTableEntry {
                                symbol: name.name.to_string(),
                                parent: current.clone(),
                                value,
                            })),
                        );
                    } else {
                        global.borrow_mut().t.insert(
                            name.name.to_string(),
                            Rc::new(RefCell::new(SymbolTableEntry {
                                symbol: name.name.to_string(),
                                parent: global.clone(),
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

    fn visit_assignment_source(
        global_scope: Rc<RefCell<SymbolTable>>,
        current_scope: Rc<RefCell<SymbolTable>>,
        value: &Expression,
    ) -> SymbolValue {
        match value {
            Expression::Nil(_) => SymbolValue::NilLit,
            Expression::False(_) => SymbolValue::BoolLit(false),
            Expression::True(_) => SymbolValue::BoolLit(true),
            Expression::Numeral(n) => {
                let v = n.numeral.parse::<f32>().unwrap();
                SymbolValue::NumberLit(v)
            }
            Expression::LiteralString(s) => SymbolValue::StringLit(s.value.to_vec()),
            Expression::Name(n) => {
                if let Some(global) = current_scope.borrow().find(&*n.name) {
                    SymbolValue::Symbol(global.clone())
                } else {
                    SymbolValue::Name(n.name.to_string())
                }
            }
            Expression::VarArgs(_) => SymbolValue::VarArgs,
            Expression::FunctionDef(def) => {
                let args = Rc::new(RefCell::new(Vec::new()));
                let scope = Rc::new(RefCell::new(SymbolTable::default()));
                scope.borrow_mut().parent = Some(current_scope.clone());
                for v in &def.par_list.parts {
                    match v {
                        ParListPart::Comma(_) => continue,
                        ParListPart::Name(v) => {
                            args.borrow_mut()
                                .push((v.name.to_string(), SymbolValue::None));
                        }
                        ParListPart::VarArgs(_v) => {
                            args.borrow_mut()
                                .push(("...".to_string(), SymbolValue::VarArgs));
                        }
                    }
                }
                for stmt in &def.block.0 {
                    Self::visit_stmt_continued(global_scope.clone(), scope.clone(), stmt)
                }
                SymbolValue::FunctionDecl { args, scope }
            }
            Expression::TableCtor(table_lit) => {
                let t = Rc::new(RefCell::new(SymbolTable::default()));
                t.borrow_mut().parent = Some(current_scope.clone());
                for (idx, entry) in table_lit.field_list.iter().enumerate() {
                    match entry {
                        Field::Record {
                            name,
                            eq: _,
                            value,
                            sep: _,
                        } => {
                            let Expression::Name(name) = name else {
                                panic!("non-name key: {name:?}")
                            };
                            let v = Self::visit_assignment_source(
                                current_scope.clone(),
                                t.clone(),
                                value,
                            );
                            let entry = SymbolTableEntry {
                                parent: t.clone(),
                                symbol: name.name.to_string(),
                                value: v,
                            };
                            t.borrow_mut().insert(name.name.to_string(), entry);
                        }
                        Field::List { value, sep: _ } => {
                            let value = Self::visit_assignment_source(
                                current_scope.clone(),
                                t.clone(),
                                value,
                            );
                            let symbol = format!("::{}::", idx);
                            let entry = SymbolTableEntry {
                                parent: t.clone(),
                                symbol: symbol.clone(),
                                value,
                            };
                            t.borrow_mut().insert(symbol, entry);
                        }
                    }
                }
                SymbolValue::TableLit(t)
            }
            Expression::Parened { expr, .. } => {
                Self::visit_assignment_source(global_scope, current_scope, &*expr)
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
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn build_symbol_table_happy() {
        let lua = r#"
a = 1
b = 2.5
c = "hello world"
d = true
e = false
g = function(a, b, c)
  local d = a
  local e = b
  local f = c
end
f = e
h = { a = 1, b = b, c = "jeez" }
"#;
        let mut parser = analisar::aware::Parser::new(lua.as_bytes());
        let mut visitor = SymbolTableVisitor::default();
        while let Some(stmt) = parser.next() {
            let stmt = stmt.unwrap();
            visitor.visit_stmt(&stmt.statement);
        }
        panic!("{visitor:#?}");
    }
}
