//! This module is for transforming/optimizing the lua AST
use analisar::aware::ast::{
    Args, Block, ExpListItem, Expression, Field, FuncBody, FunctionCall, Statement, Table,
};
use core::fmt;
use lex_lua::Span;
use std::collections::{BTreeMap, BTreeSet, HashMap};

struct Visitor {
    symbol_table: HashMap<String, Symbol>,
    scopes: Vec<Scope>,
}

impl Visitor {
    pub fn visit_stmt<'a>(&mut self, stmt: Statement<'a>) -> Statement<'a> {
        match stmt {
            Statement::Empty(span) => Statement::Empty(span),
            Statement::Expression(expr) => Statement::Expression(self.visit_expression_stmt(expr)),
            Statement::Assignment {
                targets,
                values,
                local_span,
                eq_span,
            } => {
                self.visit_assignment_stmt(&targets, &values, local_span.as_ref(), eq_span.as_ref())
            }
            Statement::Label { .. } => todo!(),
            Statement::Break(_) => todo!(),
            Statement::GoTo { goto_span, label } => todo!(),
            Statement::Do {
                block,
                do_span,
                end_span,
            } => todo!(),
            Statement::While {
                exp,
                block,
                while_span,
                do_span,
                end_span,
            } => todo!(),
            Statement::Repeat {
                block,
                exp,
                repeat_span,
                until_span,
            } => todo!(),
            Statement::If(_) => todo!(),
            Statement::For(_) => todo!(),
            Statement::ForIn(_) => todo!(),
            Statement::Function {
                local,
                name,
                body,
                function,
            } => todo!(),
            Statement::Return(_) => todo!(),
        }
    }

    pub fn visit_assignment_stmt<'a>(
        &self,
        targets: &[ExpListItem<'a>],
        values: &[ExpListItem<'a>],
        local_span: Option<&Span>,
        eq_span: Option<&Span>,
    ) -> Statement<'a> {
        for target in targets {
            let ExpListItem::Expr(expr) = target else {
                continue;
            };
        }
        todo!()
    }

    fn visit_expression_stmt<'a>(&mut self, expr: Expression<'a>) -> Expression<'a> {
        match expr {
            Expression::FunctionDef(mut f) => {
                let FuncBody {
                    open_paren_span,
                    par_list,
                    close_paren_span,
                    block,
                    end_span,
                } = f;
                let mut ret = FuncBody {
                    open_paren_span,
                    par_list,
                    close_paren_span,
                    block: Block(Vec::new()),
                    end_span,
                };
                for s in block.0 {
                    ret.block.0.push(self.visit_stmt(s))
                }
                Expression::FunctionDef(ret)
            }
            Expression::TableCtor(t) => {
                let mut table = Table {
                    open_brace: t.open_brace,
                    field_list: Vec::new(),
                    close_brace: t.close_brace,
                };
                for field in t.field_list {
                    table.field_list.push(self.visit_field(field));
                }
                Expression::TableCtor(Box::new(table))
            }
            Expression::Parened {
                open_span,
                expr,
                close_span,
            } => {
                let expr = self.visit_expression_stmt(*expr);
                Expression::Parened {
                    open_span,
                    expr: Box::new(expr),
                    close_span,
                }
            }
            Expression::BinOp { left, op, right } => {
                let left = self.visit_expression(*left);
                let right = self.visit_expression(*right);
                Expression::BinOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                }
            }
            Expression::UnaryOp { op, exp } => {
                let exp = self.visit_expression(*exp);
                Expression::UnaryOp {
                    op,
                    exp: Box::new(exp),
                }
            }
            Expression::FuncCall(call) => {
                let FunctionCall { args, prefix } = call;
                let args = Self::args_to_list(args);
                Expression::FuncCall(FunctionCall { args, prefix })
            }
            Expression::Suffixed(_) => todo!(),
            ret => ret,
        }
    }

    fn args_to_list<'a>(args: Args<'a>) -> Args<'a> {
        match args {
            Args::Table(t) => Args::ExpList {
                open_paren: t.open_brace.clone(),
                close_paren: t.close_brace.clone(),
                exprs: vec![ExpListItem::Expr(Expression::TableCtor(Box::new(t)))],
            },
            Args::String(s) => Args::ExpList {
                open_paren: s.span.clone(),
                close_paren: s.span.clone(),
                exprs: vec![ExpListItem::Expr(Expression::LiteralString(s))],
            },
            args => args,
        }
    }

    fn visit_field<'a>(&self, f: Field<'a>) -> Field<'a> {
        todo!()
    }

    fn visit_expression<'a>(&self, expr: Expression<'a>) -> Expression<'a> {
        todo!()
    }
}

struct Scope {
    parent_idx: usize,
}

struct Symbol {
    scope_idx: usize,
    assigned: Vec<ConstOrSymbol>,
}

enum ConstOrSymbol {
    Const(Const),
    Symbol(Symbol),
}

enum Const {
    Nil,
    Bool(bool),
    Number(f32),
    String(String),
}
