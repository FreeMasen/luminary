use core::{cmp, fmt};
use std::collections::{BTreeMap, BTreeSet, HashMap};

use crate::Error;
use analisar::ast::{Args, BinaryOperator, Expression, Field, Statement, UnaryOperator};

fn resolve_ids(stmts: Vec<Statement>) -> BTreeMap<String, DataType> {
    let mut known_ids = BTreeMap::new();
    for stmt in stmts {
        match stmt {
            Statement::Assignment {
                local,
                targets,
                values,
            } => {
                println!("{targets:?}");
                for (idx, expr) in targets.iter().enumerate() {
                    let Some(id) = expr_to_key(&expr, &known_ids)  else {
                        eprintln!("{expr:?} cannot be resolved into an id");
                        continue;
                    };
                    let dt = values
                        .get(idx)
                        .map(|e| expr_to_data_type(e, &mut known_ids))
                        .unwrap_or(DataType::Any);
                    known_ids.insert(id, dt);
                }
            }
            _ => todo!(),
        }
    }
    known_ids
}

fn expr_to_data_type(expr: &Expression, known_ids: &mut BTreeMap<String, DataType>) -> DataType {
    match expr {
        Expression::Nil => DataType::Nil,
        Expression::False | Expression::True => DataType::Bool,
        Expression::Numeral(_) => DataType::Number,
        Expression::LiteralString(_) => DataType::String,
        Expression::Name(name) => known_ids
            .get(name.name.as_ref())
            .cloned()
            .unwrap_or(DataType::Any),
        Expression::VarArgs => todo!(),
        Expression::FunctionDef(func) => {
            let mut args = Vec::new();
            let mut arg_aliases = BTreeMap::new();
            let mut shadows = BTreeMap::new();
            for (idx, name) in func.par_list.names.0.iter().enumerate() {
                arg_aliases.insert(name.name.to_string(), idx);
                args.push(DataType::Any);
            }
            if func.par_list.var_args {
                arg_aliases.insert("...".to_string(), func.par_list.names.0.len());
                args.push(DataType::Multi(Vec::new()));
            }
            let mut rets = DataType::Any;
            for stmt in &func.block.0 {
                try_type_args(stmt, &mut args, &mut arg_aliases, &mut shadows, known_ids);
                if let Statement::Return(ret) = stmt {
                    let mut inner_rets: Vec<DataType> = ret
                        .0
                        .iter()
                        .map(|e| expr_to_data_type(e, known_ids))
                        .collect();
                    if inner_rets.len() == 1 {
                        rets = rets.join(inner_rets.pop().unwrap())
                    } else {
                        rets.push(DataType::Multi(inner_rets))
                    }
                }
            }
            if matches!(rets, DataType::Any) {
                rets = DataType::Nil;
            }
            DataType::Function(FunctionDataType {
                args: args,
                rets: Box::new(rets),
                arg_aliases,
            })
        }
        Expression::TableCtor(table) => {
            let mut props = BTreeMap::new();
            for (idx, field) in table.field_list.iter().enumerate() {
                match field {
                    Field::Record { name, value } => {
                        let key = expr_to_key(name, known_ids)
                            .unwrap_or_else(|| format!("unknown_prop_{idx}"));
                        let dt = expr_to_data_type(value, known_ids);
                        props.insert(key, dt);
                    }
                    Field::List(expr) => {
                        props.insert(idx.to_string(), expr_to_data_type(expr, known_ids));
                    }
                }
            }
            DataType::Table { props }
        }
        Expression::BinOp {
            left: _,
            op,
            right: _,
        } => bin_op_to_data_type(op),
        Expression::UnaryOp { op, exp: _ } => un_op_to_dt(op),
        Expression::FuncCall(call) => {
            if let Some(id) = expr_to_key(&call.prefix, known_ids) {
                if let Some(known) = known_ids.get(&id) {
                    if let DataType::Function(FunctionDataType { rets, .. }) = known {
                        return rets.as_ref().clone();
                    }
                }
            }
            DataType::Any
        }
        Expression::Suffixed(suffixed) => {
            // if the subject is resolvable to a string
            if let Some(id) = expr_to_key(&suffixed.subject, known_ids) {
                // if the subject is a know id
                if let Some(known_type) = known_ids.get(&id) {
                    // if the subject maps to a known table
                    if let DataType::Table { props } = known_type {
                        // if the expression can be resolve to a key in the table
                        // TODO: handle nested Suffixed expressions...
                        if let Some(key) = expr_to_key(&suffixed.property, known_ids) {
                            // if the table contains the property key
                            if let Some(prop_type) = props.get(&key) {
                                return prop_type.clone();
                            }
                        }
                    }
                }
            }
            DataType::Any
        }
    }
}

fn try_type_args(
    stmt: &Statement,
    args: &mut Vec<DataType>,
    arg_aliases: &mut BTreeMap<String, usize>,
    shadows: &mut BTreeMap<String, DataType>,
    known_ids: &mut BTreeMap<String, DataType>,
) {
    println!("try_type_args {stmt:?}");
    match stmt {
        Statement::Empty => return,
        Statement::Expression(expr) => {
            try_type_args_from_expr(expr, args, arg_aliases, shadows, known_ids)
        }
        Statement::Assignment {
            local,
            targets,
            values,
        } => for (idx, target) in targets.iter().enumerate() {
            if let Some(target) = expr_to_key(target, known_ids) {
                let dt = values.get(idx).map(|e| expr_to_data_type(e, known_ids)).unwrap_or(DataType::Any);
                if local {
                    shadows.insert(target,dt);
                } else {
                    if let Some(known) = known_ids.get_mut(&target) {
                        known.push(dt);
                    }
                }
            }
        },
        Statement::Label(_) => todo!(),
        Statement::Break => todo!(),
        Statement::GoTo(_) => todo!(),
        Statement::Do { block } => todo!(),
        Statement::While { exp, block } => todo!(),
        Statement::Repeat { block, exp } => todo!(),
        Statement::If(_) => todo!(),
        Statement::For(_) => todo!(),
        Statement::ForIn(_) => todo!(),
        Statement::Function { local, name, body } => todo!(),
        Statement::Return(rets) => {
            for expr in &rets.0 {
                try_type_args_from_expr(expr, args, arg_aliases, shadows, known_ids)
            }
        }
    }
}

fn try_type_args_from_expr(
    expr: &Expression,
    args: &mut Vec<DataType>,
    arg_aliases: &mut BTreeMap<String, usize>,
    shadows: &mut BTreeMap<String, DataType>,
    known_ids: &mut BTreeMap<String, DataType>,
) {
    println!("try_type_args_from_expr {expr:?}");
    match expr {
        Expression::BinOp { left, op, right } => {
            if let Expression::Name(left_name) = left.as_ref() {
                if !shadows.contains_key(left_name.name.as_ref()) {
                    if let Some(idx) = arg_aliases.get(left_name.name.as_ref()) {
                        let arg = args.get_mut(*idx).expect("Invalid function args");
                        arg.push(bin_op_to_data_type(op));
                    }
                }
            }
            if let Expression::Name(right_name) = right.as_ref() {
                if !shadows.contains_key(right_name.name.as_ref()) {
                    if let Some(idx) = arg_aliases.get(right_name.name.as_ref()) {
                        let arg = args.get_mut(*idx).expect("Invalid function args");
                        arg.push(bin_op_to_data_type(op));
                    }
                }
            }
        }
        Expression::UnaryOp { op, exp } => {
            if let Expression::Name(op_name) = exp.as_ref() {
                if !shadows.contains_key(op_name.name.as_ref()) {
                    if let Some(idx) = arg_aliases.get(op_name.name.as_ref()) {
                        let arg = args.get_mut(*idx).expect("Invalid function args");
                        arg.push(un_op_to_dt(op));
                    }
                }
            }
        }
        Expression::FuncCall(call) => {
            match call.prefix.as_ref() {
                Expression::Name(fn_name) => {
                    if let Some(shadowed) = shadows.get_mut(fn_name.name.as_ref()) {
                        // TODO update shadowed function def?
                        return;
                    }
                    if let Some(idx) = arg_aliases.get(fn_name.name.as_ref()) {
                        let arg = args.get_mut(*idx).expect("invalid function args");
                        match &call.args {
                            Args::ExpList(list) => {
                                for (idx, exp) in list.iter().enumerate() {
                                    let mut temp_ki: BTreeMap<String, DataType> = known_ids
                                        .clone()
                                        .into_iter()
                                        .chain(shadows.clone().into_iter())
                                        .collect();
                                    if let Some(arg) = args.get_mut(idx) {
                                        arg.push(expr_to_data_type(expr, &mut temp_ki));
                                    } else {
                                        args.push(expr_to_data_type(expr, &mut temp_ki));
                                    }
                                }
                            }
                            Args::Table(t) => {
                                if let Some(arg) = args.get_mut(0) {
                                    args.push(DataType::Table {
                                        props: BTreeMap::new(),
                                    })
                                }
                            }
                            Args::String(s) => {
                                if let Some(arg) = args.get_mut(0) {
                                    arg.push(DataType::String);
                                } else {
                                    args.push(DataType::String);
                                }
                            }
                        }
                    }
                    // if let Some(argarg) = args.iter_mut().find(|name _| name == fn_name.name.as_ref()) {

                    //     match call.args {
                    //         Args::ExpList(exprs) => {

                    //         },
                    //         Args::Table(t) => {},
                    //         Args::String(_) => {}
                    //     }
                    // }
                    if let Some(known) = known_ids.get_mut(fn_name.name.as_ref()) {
                        // TODO: update global function def?
                        return;
                    }
                }
                _ => {}
            }
        }
        Expression::Suffixed(_) => todo!(),
        _ => return,
    }
}

fn expr_to_key(expr: &Expression, known_ids: &BTreeMap<String, DataType>) -> Option<String> {
    match expr {
        Expression::Nil => todo!(),
        Expression::False => todo!(),
        Expression::True => todo!(),
        Expression::Numeral(num) => todo!(),
        Expression::LiteralString(lit) => Some(String::from_utf8_lossy(&lit.0).to_string()),
        Expression::Name(name) => Some(name.name.to_string()),
        Expression::VarArgs => Some("...".to_string()),
        Expression::FunctionDef(_) | Expression::TableCtor(_) => None,
        Expression::BinOp { left, op, right } => todo!(),
        Expression::UnaryOp { op, exp } => todo!(),
        Expression::FuncCall(_) => todo!(),
        Expression::Suffixed(_) => todo!(),
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum DataType {
    Any,
    Nil,
    Bool,
    Number,
    String,
    Table { props: BTreeMap<String, DataType> },
    Thread,
    UserData,
    Function(FunctionDataType),
    Multi(Vec<DataType>),
    Union(BTreeSet<DataType>),
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord)]
pub struct FunctionDataType {
    args: Vec<DataType>,
    arg_aliases: BTreeMap<String, usize>,
    rets: Box<DataType>,
}

impl FunctionDataType {
    fn join(&mut self, other: Self) {
        let mut right_iter = other.args.into_iter();
        {
            let mut left_iter = self.args.iter_mut();
            while let Some(left_arg) = left_iter.next() {
                let right_arg = right_iter.next().unwrap_or(DataType::Nil);
                left_arg.push(right_arg);
            }
        }
        while let Some(mut right_arg) = right_iter.next() {
            right_arg.push(DataType::Nil);
            self.args.push(right_arg);
        }
        self.rets.push(*other.rets);
    }
}

impl fmt::Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DataType::Any => write!(f, "any"),
            DataType::Nil => write!(f, "nil"),
            DataType::Bool => write!(f, "bool"),
            DataType::Number => write!(f, "number"),
            DataType::String => write!(f, "string"),
            DataType::Table { props } => {
                write!(f, "{{")?;
                let mut after_first = false;
                for (k, v) in props {
                    if after_first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{k}: {v}")?;
                    after_first = true;
                }
                write!(f, "}}")
            }
            DataType::Thread => write!(f, "thread"),
            DataType::UserData => write!(f, "userdata"),
            DataType::Function(FunctionDataType {
                args,
                arg_aliases,
                rets,
            }) => {
                write!(f, "fun(")?;
                let mut after_first = false;
                for (idx, v) in args.iter().enumerate() {
                    if after_first {
                        write!(f, ", ")?;
                    }
                    let mut after_first_name = false;
                    for (k, _) in arg_aliases.iter().filter(|(k, v)| **v == idx) {
                        if after_first_name {
                            write!(f, "|")?;
                        }
                        write!(f, "{k}")?;
                        after_first_name = true;
                    }
                    write!(f, ": {v}")?;
                    after_first = true;
                }
                write!(f, ")")?;
                after_first = false;
                if !matches!(rets.as_ref(), &DataType::Nil) {
                    write!(f, " -> {rets}")?
                }
                Ok(())
            }
            DataType::Multi(types) => {
                let mut after_first = false;
                for t in types {
                    if after_first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{t}")?;
                    after_first = true;
                }
                Ok(())
            }
            DataType::Union(types) => {
                let mut after_first = false;
                for t in types {
                    if after_first {
                        write!(f, "|")?;
                    }
                    write!(f, "{t}")?;
                    after_first = true;
                }
                Ok(())
            }
        }
    }
}

fn bin_op_to_data_type(op: &BinaryOperator) -> DataType {
    match op {
        BinaryOperator::Add
        | BinaryOperator::Subtract
        | BinaryOperator::Multiply
        | BinaryOperator::Divide
        | BinaryOperator::FloorDivide
        | BinaryOperator::Power
        | BinaryOperator::Modulo
        | BinaryOperator::BitwiseAnd
        | BinaryOperator::BitwiseXor
        | BinaryOperator::BitwiseOr
        | BinaryOperator::RightShift
        | BinaryOperator::LeftShift => DataType::Number,
        BinaryOperator::Concatenate => DataType::String,
        BinaryOperator::GreaterThan
        | BinaryOperator::GreaterThanEqual
        | BinaryOperator::LessThan
        | BinaryOperator::LessThanEqual
        | BinaryOperator::Equal
        | BinaryOperator::NotEqual
        | BinaryOperator::And
        | BinaryOperator::Or => DataType::Bool,
    }
}

fn un_op_to_dt(op: &UnaryOperator) -> DataType {
    match op {
        UnaryOperator::Negate => DataType::Number,
        UnaryOperator::Not => DataType::Bool,
        UnaryOperator::Length => DataType::Number,
        UnaryOperator::BitwiseNot => DataType::Number,
    }
}

impl DataType {
    /// Union a type with this type
    pub fn join(self, other: Self) -> Self {
        if matches!(self, Self::Any) {
            return other;
        }
        let mut set = if let Self::Union(set) = self {
            set
        } else {
            let mut set = BTreeSet::new();
            set.insert(self.clone());
            set
        };

        if let Self::Union(mut other_list) = other {
            set.append(&mut other_list);
        } else {
            set.insert(other);
        }
        Self::Union(set)
    }

    /// Append a type with this type to a Multi
    pub fn push(&mut self, mut other: Self) {
        if matches!(self, Self::Any) {
            *self = other;
            return;
        }
        if let Self::Multi(inner) = self {
            inner.push(other);
        } else {
            if let Self::Multi(mut inner) = other {
                inner.insert(0, self.clone());
                *self = Self::Multi(inner);
            } else {
                *self = Self::Multi(vec![self.clone(), other])
            }
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::{BTreeMap, BTreeSet};

    use analisar::Parser;

    use super::*;

    #[test]
    fn fmt_data_types() {
        let expectations = &[
            (DataType::Nil, "nil"),
            (DataType::Any, "any"),
            (DataType::Number, "number"),
            (DataType::String, "string"),
            (DataType::Thread, "thread"),
            (DataType::UserData, "userdata"),
            (
                DataType::Function(FunctionDataType {
                    args: Vec::new(),
                    rets: Box::new(DataType::Nil),
                    arg_aliases: BTreeMap::new(),
                }),
                "fun()",
            ),
            (
                DataType::Function(FunctionDataType {
                    args: vec![DataType::Any],
                    rets: Box::new(DataType::Any),
                    arg_aliases: BTreeMap::from_iter([("a".to_string(), 0)].into_iter()),
                }),
                "fun(a: any) -> any",
            ),
            (
                DataType::Function(FunctionDataType {
                    args: vec![DataType::Number, DataType::String],
                    arg_aliases: BTreeMap::from_iter(
                        [("a".into(), 0), ("b".into(), 1)].into_iter(),
                    ),
                    rets: Box::new(DataType::Multi(vec![DataType::Number, DataType::String])),
                }),
                "fun(a: number, b: string) -> number, string",
            ),
            (
                DataType::Table {
                    props: BTreeMap::new(),
                },
                "{}",
            ),
            (
                DataType::Table {
                    props: BTreeMap::from_iter([("a".into(), DataType::Any)].into_iter()),
                },
                "{a: any}",
            ),
            (
                DataType::Table {
                    props: BTreeMap::from_iter(
                        [
                            ("a".into(), DataType::Number),
                            ("b".into(), DataType::String),
                        ]
                        .into_iter(),
                    ),
                },
                "{a: number, b: string}",
            ),
            (
                DataType::Union(BTreeSet::from_iter(
                    [DataType::String, DataType::Nil].into_iter(),
                )),
                "nil|string",
            ),
        ];
        for (lhs, rhs) in expectations.into_iter() {
            assert_eq!(lhs.to_string(), *rhs);
        }
    }

    #[test]
    fn check_resolve_ids() {
        let lua = r#"a = false
        b = 2
        c = "string"
        local d = {}
        e = {
            a = 1,
            b = 2,
        }
        f = function(a, b)
            return a + b
        end
        "#;
        let mut parser = Parser::new(lua.as_bytes());
        let mut stmts = Vec::new();
        while let Some(maybe_stmt) = parser.next() {
            stmts.push(maybe_stmt.unwrap());
        }
        let vars = resolve_ids(stmts);
        assert_eq!(vars.get("a").cloned().unwrap(), DataType::Bool);
        assert_eq!(vars.get("b").cloned().unwrap(), DataType::Number);
        assert_eq!(vars.get("c").cloned().unwrap(), DataType::String);
        assert_eq!(
            vars.get("d").cloned().unwrap(),
            DataType::Table {
                props: Default::default()
            }
        );
        assert_eq!(
            vars.get("e").cloned().unwrap(),
            DataType::Table {
                props: [
                    ("a".into(), DataType::Number),
                    ("b".into(), DataType::Number),
                ]
                .into_iter()
                .collect()
            }
        );
        assert_eq!(
            vars.get("f").cloned().unwrap(),
            DataType::Function(FunctionDataType {
                args: vec![DataType::Number, DataType::Number],
                arg_aliases: [("a".into(), 0), ("b".into(), 1),].into_iter().collect(),
                rets: Box::new(DataType::Number)
            })
        );
    }
}
