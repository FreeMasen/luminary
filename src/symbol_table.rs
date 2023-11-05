//TODO: this module isn't done so things are still unused
#![allow(dead_code, unused)]
use analisar::aware::ast::{ExpListItem, Expression, Field, ParListPart, Statement};
use indexmap::IndexMap;
use lex_lua::Span;
use std::{cell::RefCell, fmt::Debug, rc::Rc};

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
    details: SymbolDetails,
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
        args: Rc<RefCell<SymbolTable>>,
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
                struct FunctionArguments<'a>(&'a SymbolTable);
                impl<'a> Debug for FunctionArguments<'a> {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        let mut d = f.debug_struct("");
                        for (k, v) in self.0.t.iter() {
                            d.field(k, &v.borrow().value);
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
    pub fn new(
        symbol: String,
        parent: Rc<RefCell<SymbolTable>>,
        value: SymbolValue,
        details: SymbolDetails,
    ) -> Self {
        Self {
            symbol,
            parent,
            value,
            details,
        }
    }
}

#[derive(Clone)]
pub struct SymbolDetails {
    start_byte_offset: usize,
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
        Self::visit_stmt_continued(
            self.global.clone(),
            self.module.clone(),
            &mut self.scopes,
            stmt,
        )
    }
    fn visit_stmt_continued(
        global: Rc<RefCell<SymbolTable>>,
        current: Rc<RefCell<SymbolTable>>,
        scopes: &mut Vec<Rc<RefCell<SymbolTable>>>,
        stmt: &Statement,
    ) {
        match stmt {
            Statement::Empty(_) => {}
            Statement::Expression(expr) => {
                Self::visit_expr_stmt_continued(global.clone(), current.clone(), scopes, expr)
            }
            Statement::Assignment {
                local_span,
                targets,
                eq_span: _,
                values,
            } => Self::visit_assignment_continued(
                global.clone(),
                current.clone(),
                scopes,
                local_span.as_ref(),
                targets,
                values,
            ),
            Statement::Label {
                colons1_span, name, ..
            } => {
                current.borrow_mut().insert(
                    name.name.to_string(),
                    SymbolTableEntry {
                        parent: current.clone(),
                        symbol: name.name.to_string(),
                        value: SymbolValue::Label(name.name.to_string()),
                        details: SymbolDetails {
                            start_byte_offset: colons1_span.start,
                        },
                    },
                );
            }
            Statement::Break(_) => todo!(),
            Statement::GoTo { .. } => todo!(),
            Statement::Do { block, .. } => {
                let mut do_scope = SymbolTable::default();
                do_scope.parent = Some(current.clone());
                let do_scope = Rc::new(RefCell::new(do_scope));
                for stmt in block.0.iter() {
                    Self::visit_stmt_continued(global.clone(), do_scope.clone(), scopes, stmt);
                }
                scopes.push(do_scope);
            }
            Statement::While { .. } => todo!(),
            Statement::Repeat { .. } => todo!(),
            Statement::If(_) => todo!(),
            Statement::For(_) => todo!(),
            Statement::ForIn(_) => todo!(),
            Statement::Function { .. } => todo!(),
            Statement::Return(_) => todo!(),
        }
    }

    fn visit_expr_stmt_continued(
        _global: Rc<RefCell<SymbolTable>>,
        _current: Rc<RefCell<SymbolTable>>,
        _scopes: &mut Vec<Rc<RefCell<SymbolTable>>>,
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
            Expression::Parened { .. } => todo!(),
            Expression::BinOp { .. } => todo!(),
            Expression::UnaryOp { .. } => todo!(),
            Expression::FuncCall(_) => todo!(),
            Expression::Suffixed(_) => todo!(),
        }
    }

    fn visit_assignment_continued(
        global: Rc<RefCell<SymbolTable>>,
        current: Rc<RefCell<SymbolTable>>,
        scopes: &mut Vec<Rc<RefCell<SymbolTable>>>,
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

            let value =
                Self::visit_assignment_source(global.clone(), current.clone(), scopes, value);
            match target {
                Expression::Name(name) => {
                    if local_span.is_some() {
                        current.borrow_mut().t.insert(
                            name.name.to_string(),
                            Rc::new(RefCell::new(SymbolTableEntry {
                                symbol: name.name.to_string(),
                                parent: current.clone(),
                                value,
                                details: SymbolDetails {
                                    start_byte_offset: name.start(),
                                },
                            })),
                        );
                    } else {
                        global.borrow_mut().t.insert(
                            name.name.to_string(),
                            Rc::new(RefCell::new(SymbolTableEntry {
                                symbol: name.name.to_string(),
                                parent: global.clone(),
                                value,
                                details: SymbolDetails {
                                    start_byte_offset: name.start(),
                                },
                            })),
                        );
                    }
                }
                Expression::Suffixed(_expr) => {
                    todo!("Assigning to suffixed");
                }
                _ => panic!("assigning to non-name or suffixed expression"),
            }
        }
    }

    fn visit_assignment_source(
        global_scope: Rc<RefCell<SymbolTable>>,
        current_scope: Rc<RefCell<SymbolTable>>,
        scopes: &mut Vec<Rc<RefCell<SymbolTable>>>,
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
                let args = Rc::new(RefCell::new(SymbolTable::default()));
                let scope = Rc::new(RefCell::new(SymbolTable::default()));
                args.borrow_mut().parent = Some(current_scope.clone());
                scope.borrow_mut().parent = Some(args.clone());
                scopes.push(scope.clone());
                for v in &def.par_list.parts {
                    match v {
                        ParListPart::Comma(_) => continue,
                        ParListPart::Name(v) => {
                            args.borrow_mut().insert(
                                v.name.to_string(),
                                SymbolTableEntry {
                                    parent: current_scope.clone(),
                                    symbol: v.name.to_string(),
                                    value: SymbolValue::None,
                                    details: SymbolDetails {
                                        start_byte_offset: v.start(),
                                    },
                                },
                            );
                        }
                        ParListPart::VarArgs(v) => {
                            args.borrow_mut().insert(
                                "...".to_string(),
                                SymbolTableEntry {
                                    parent: current_scope.clone(),
                                    symbol: "...".to_string(),
                                    value: SymbolValue::VarArgs,
                                    details: SymbolDetails {
                                        start_byte_offset: v.start,
                                    },
                                },
                            );
                        }
                    }
                }
                for stmt in &def.block.0 {
                    Self::visit_stmt_continued(global_scope.clone(), scope.clone(), scopes, stmt)
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
                                scopes,
                                value,
                            );
                            let entry = SymbolTableEntry {
                                parent: t.clone(),
                                symbol: name.name.to_string(),
                                value: v,
                                details: SymbolDetails {
                                    start_byte_offset: name.start(),
                                },
                            };
                            t.borrow_mut().insert(name.name.to_string(), entry);
                        }
                        Field::List { value: exp, sep: _ } => {
                            let value = Self::visit_assignment_source(
                                current_scope.clone(),
                                t.clone(),
                                scopes,
                                exp,
                            );
                            let symbol = format!("::{}::", idx);
                            let entry = SymbolTableEntry {
                                parent: t.clone(),
                                symbol: symbol.clone(),
                                value,
                                details: SymbolDetails {
                                    start_byte_offset: exp.start(),
                                },
                            };
                            t.borrow_mut().insert(symbol, entry);
                        }
                    }
                }
                SymbolValue::TableLit(t)
            }
            Expression::Parened { expr, .. } => {
                Self::visit_assignment_source(global_scope, current_scope, scopes, &*expr)
            }
            Expression::BinOp { left: _, op: _, right: _ } => {
                todo!()
            }
            Expression::UnaryOp { op: _, exp: _ } => {
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
    #[should_panic]
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
do
    local a = 1
    local b = "2"
    local c = false
end
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
