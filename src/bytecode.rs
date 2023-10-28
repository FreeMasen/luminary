//! This module is for transforming/optimizing the lua AST
use analisar::aware::ast::{
    Args, Block, ExpListItem, Expression, Field, FuncBody, FunctionCall, Statement, Table,
};
use lex_lua::Span;
use std::{collections::HashMap};

pub struct Visitor {
    pub symbol_table: HashMap<String, Symbol>,
    pub scopes: Vec<Scope>,
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
            Statement::GoTo { .. } => todo!(),
            Statement::Do { .. } => todo!(),
            Statement::While { .. } => todo!(),
            Statement::Repeat { .. } => todo!(),
            Statement::If(_) => todo!(),
            Statement::For(_) => todo!(),
            Statement::ForIn(_) => todo!(),
            Statement::Function { .. } => todo!(),
            Statement::Return(_) => todo!(),
        }
    }

    pub fn visit_assignment_stmt<'a>(
        &self,
        targets: &[ExpListItem<'a>],
        _values: &[ExpListItem<'a>],
        _local_span: Option<&Span>,
        _eq_span: Option<&Span>,
    ) -> Statement<'a> {
        for target in targets {
            let ExpListItem::Expr(_) = target else {
                continue;
            };
        }
        todo!()
    }

    fn visit_expression_stmt<'a>(&mut self, expr: Expression<'a>) -> Expression<'a> {
        match expr {
            Expression::FunctionDef(f) => {
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

    fn args_to_list(args: Args) -> Args {
        match args {
            Args::Table(t) => Args::ExpList {
                open_paren: t.open_brace,
                close_paren: t.close_brace,
                exprs: vec![ExpListItem::Expr(Expression::TableCtor(Box::new(t)))],
            },
            Args::String(s) => Args::ExpList {
                open_paren: s.span,
                close_paren: s.span,
                exprs: vec![ExpListItem::Expr(Expression::LiteralString(s))],
            },
            args => args,
        }
    }

    fn visit_field<'a>(&self, _f: Field<'a>) -> Field<'a> {
        todo!()
    }

    fn visit_expression<'a>(&self, _expr: Expression<'a>) -> Expression<'a> {
        todo!()
    }
}

pub struct Scope {
    pub parent_idx: usize,
}

pub struct Symbol {
    pub scope_idx: usize,
    pub assigned: Vec<ConstOrSymbol>,
}

pub enum ConstOrSymbol {
    Const(Const),
    Symbol(Symbol),
}

pub enum Const {
    Nil,
    Bool(bool),
    Number(f32),
    String(String),
}
