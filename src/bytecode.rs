use core::fmt;
use std::collections::{BTreeMap, BTreeSet};

use analisar::ast::{Args, BinaryOperator, Expression, Field, Statement, UnaryOperator};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicType, StructType},
    values::AnyValue,
    AddressSpace,
};

fn resolve_ids(stmts: Vec<Statement>) -> BTreeMap<String, DataType> {
    let mut known_ids = BTreeMap::new();
    for stmt in stmts {
        match stmt {
            Statement::Assignment {
                local: _,
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
        } => {
            for (idx, target) in targets.iter().enumerate() {
                if let Some(target) = expr_to_key(target, known_ids) {
                    let dt = values
                        .get(idx)
                        .map(|e| expr_to_data_type(e, known_ids))
                        .unwrap_or(DataType::Any);
                    if *local {
                        shadows.insert(target, dt);
                    } else {
                        if let Some(known) = known_ids.get_mut(&target) {
                            known.push(dt);
                        }
                    }
                }
            }
        }
        Statement::Label(_) | Statement::Break | Statement::GoTo(_) => {}
        Statement::Do { block } => {
            for stmt in &block.0 {
                try_type_args(stmt, args, arg_aliases, shadows, known_ids);
            }
        }
        Statement::While { exp, .. } => {
            if let Some(name) = expr_to_key(exp, known_ids) {
                if let Some(shadow) = shadows.get_mut(&name) {
                    shadow.push(DataType::Bool);
                } else if let Some(idx) = arg_aliases.get(&name) {
                    let arg = args.get_mut(*idx).expect("invalid function args");
                    arg.push(DataType::Bool);
                }
            }
        }
        Statement::Repeat { .. } => todo!(),
        Statement::If(_) => todo!(),
        Statement::For(_) => todo!(),
        Statement::ForIn(_) => todo!(),
        Statement::Function { .. } => todo!(),
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
                    if let Some(_shadowed) = shadows.get_mut(fn_name.name.as_ref()) {
                        // TODO update shadowed function def?
                        return;
                    }
                    if let Some(idx) = arg_aliases.get(fn_name.name.as_ref()) {
                        match &call.args {
                            Args::ExpList(list) => {
                                if let Some(exp) = list.get(*idx) {
                                    let mut temp_ki: BTreeMap<String, DataType> = known_ids
                                        .clone()
                                        .into_iter()
                                        .chain(shadows.clone().into_iter())
                                        .collect();
                                    if let Some(arg) = args.get_mut(*idx) {
                                        arg.push(expr_to_data_type(exp, &mut temp_ki));
                                    } else {
                                        args.push(expr_to_data_type(exp, &mut temp_ki));
                                    }
                                }
                            }
                            Args::Table(_t) => {
                                if let Some(arg) = args.get_mut(0) {
                                    arg.push(DataType::Table {
                                        props: BTreeMap::new(),
                                    })
                                }
                            }
                            Args::String(_s) => {
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
                    if let Some(_known) = known_ids.get_mut(fn_name.name.as_ref()) {
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

fn expr_to_key(expr: &Expression, _known_ids: &BTreeMap<String, DataType>) -> Option<String> {
    #[allow(unused_variables, dead_code)]
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
    pub fn join(&mut self, other: Self) {
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
                    for (k, _) in arg_aliases.iter().filter(|(_, v)| **v == idx) {
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

pub enum TValue {
    Nil,
    Bool(bool),
    Number(f32),
    String(Vec<u8>),
    Table(Vec<TableField>),
    Function { id: u32 },
    Thread { id: u32 },
    UserData { id: u32 },
}

impl TValue {
    pub(crate) const BASE_TYPE_NAME: &str = "enum.TValue";
    pub(crate) const GET_TAG_FN_NAME: &str = "getTValueTag";

    fn tvalue_type<'ctx>(context: &'ctx Context) -> StructType<'ctx> {
        context.struct_type(
            &[
                context.i8_type().into(),
                context.i8_type().array_type(7).into(),
            ],
            false,
        )
    }
    fn tvalue_nil_type<'ctx>(context: &'ctx Context) -> StructType<'ctx> {
        context.struct_type(&[context.i8_type().into()], false)
    }

    fn tvalue_bool_type<'ctx>(context: &'ctx Context) -> StructType<'ctx> {
        context.struct_type(
            &[
                context.i8_type().array_type(1).into(),
                context.i8_type().into(),
            ],
            false,
        )
    }

    fn tvalue_number_type<'ctx>(context: &'ctx Context) -> StructType<'ctx> {
        context.struct_type(
            &[
                context.i32_type().array_type(1).into(),
                context.f32_type().into(),
            ],
            false,
        )
    }

    fn tvalue_table_type<'ctx>(context: &'ctx Context) -> StructType<'ctx> {
        context.struct_type(
            &[
                context.i32_type().array_type(1).into(),
                context.i32_type().into(),
            ],
            false,
        )
    }

    fn tvalue_func_type<'ctx>(context: &'ctx Context) -> StructType<'ctx> {
        context.struct_type(
            &[
                context.i8_type().into(),
                // args length
                context.i8_type().into(),
                // args
                Self::tvalue_type(context)
                    .array_type(0)
                    .ptr_type(AddressSpace::from(0u16))
                    .into(),
            ],
            false,
        )
    }

    pub fn gen_lib<'ctx>(context: &'ctx Context) -> Module<'ctx> {
        let module = context.create_module("__intrinsics_tvalue");

        let builder = context.create_builder();
        Self::add_get_tag_function(context, &module, &builder);
        Self::add_get_value_bool(context, &module, &builder);
        Self::add_new_nil(context, &module, &builder);
        Self::add_new_bool(context, &module, &builder);
        module.verify().unwrap();
        module
    }

    fn add_new_bool<'ctx>(context: &'ctx Context, module: &Module<'ctx>, builder: &Builder<'ctx>) {
        let enum_base_ty = Self::tvalue_type(context);
        let f = enum_base_ty
            .ptr_type(0u16.into())
            .fn_type(&[context.bool_type().into()], false);
        let f = module.add_function("enum_TValue_bool_new", f, None);
        let entry = context.append_basic_block(f.clone(), "entry");
        builder.position_at_end(entry);
        let ret = builder.build_alloca(enum_base_ty, "ret");
        let tag_ptr = unsafe {
            builder.build_in_bounds_gep(
                ret.get_type(),
                ret,
                &[
                    context.i32_type().const_int(0, false),
                    context.i32_type().const_int(0, false),
                ],
                "tag_ptr",
            )
        };
        builder.build_store(tag_ptr, context.i8_type().const_int(1, false));
        let casted = builder.build_bitcast(
            ret,
            Self::tvalue_bool_type(context).ptr_type(0u16.into()),
            "casted",
        );
        let gep = unsafe {
            builder.build_in_bounds_gep(
                casted.get_type(),
                casted.into_pointer_value(),
                &[
                    context.i32_type().const_int(0, false),
                    context.i32_type().const_int(1, false),
                ],
                "bool_ptr",
            )
        };
        let arg = f.get_param_iter().next().expect("new bool has 1 argument");
        arg.set_name("is_true");
        let wide_arg =
            builder.build_int_z_extend(arg.into_int_value(), context.i8_type(), "wide_arg");
        builder.build_store(gep, wide_arg);
        builder.build_return(Some(&ret));
    }

    fn add_new_nil<'ctx>(context: &'ctx Context, module: &Module<'ctx>, builder: &Builder<'ctx>) {
        let enum_base_ty = Self::tvalue_type(context);
        let f = enum_base_ty.ptr_type(0u16.into()).fn_type(&[], false);
        let f = module.add_function("enum_TValue_nil_new", f, None);
        let entry = context.append_basic_block(f.clone(), "entry");
        builder.position_at_end(entry);
        let ret = builder.build_alloca(enum_base_ty, "ret");
        let tag_ptr = unsafe {
            builder.build_in_bounds_gep(
                ret.get_type(),
                ret,
                &[
                    context.i32_type().const_int(0, false),
                    context.i32_type().const_int(0, false),
                ],
                "tag_ptr",
            )
        };
        builder.build_store(tag_ptr, context.i8_type().const_int(0, false));
        let val_ptr = unsafe {
            builder.build_in_bounds_gep(
                ret.get_type(),
                ret,
                &[
                    context.i32_type().const_int(0, false),
                    context.i32_type().const_int(1, false),
                ],
                "tag_ptr",
            )
        };
        builder.build_store(val_ptr, context.i8_type().array_type(7).const_zero());
        builder.build_return(Some(&ret));
    }

    fn add_get_tag_function<'ctx>(
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
    ) {
        let get_tag_ty = context.i8_type().fn_type(
            &[Self::tvalue_type(context).ptr_type(0u16.into()).into()],
            false,
        );
        let func = module.add_function(Self::GET_TAG_FN_NAME, get_tag_ty, None);
        let arg_ptr = func.get_param_iter().next().unwrap().into_pointer_value();
        arg_ptr.set_name("tagged_value");
        let entry_block = context.append_basic_block(func, "entry");
        builder.position_at_end(entry_block);
        let tag_alloc = builder.build_alloca(context.i8_type(), "tag");
        let tag_gep = unsafe {
            builder.build_in_bounds_gep(
                arg_ptr.get_type(),
                arg_ptr,
                &[
                    context.i32_type().const_int(0, false),
                    context.i32_type().const_int(0, false),
                ],
                "tag_ptr",
            )
        };
        let tag_load = builder.build_load(tag_gep.get_type(), tag_gep, "tag_val");
        builder.build_store(tag_alloc.clone(), tag_load);
        let val = builder.build_load(tag_alloc.get_type(), tag_alloc, "ret_val");
        builder.build_return(Some(&val));
    }

    fn add_get_value_bool<'ctx>(
        context: &'ctx Context,
        module: &Module<'ctx>,
        builder: &Builder<'ctx>,
    ) {
        let get_tag_ty = context.bool_type().fn_type(
            &[Self::tvalue_type(context).ptr_type(0u16.into()).into()],
            false,
        );
        let func = module.add_function("getTValueValue_bool", get_tag_ty, None);
        let arg_ptr = func.get_param_iter().next().unwrap().into_pointer_value();
        arg_ptr.set_name("tagged_value");
        let entry_block = context.append_basic_block(func.clone(), "entry");
        builder.position_at_end(entry_block.clone());
        let get_tag_func = module
            .get_function(Self::GET_TAG_FN_NAME)
            .expect("getTagFn");
        let call = builder.build_call(get_tag_func, &[arg_ptr.into()], "tag");
        let cmp_tag = call.as_any_value_enum();
        let is_nil = builder.build_int_compare(
            inkwell::IntPredicate::EQ,
            cmp_tag.into_int_value(),
            context.i8_type().const_int(0, false),
            "is_nil",
        );
        let is_nil_block = context.append_basic_block(func.clone(), "is_nil");
        let is_not_nil_block = context.append_basic_block(func.clone(), "is_not_nil");
        builder.build_conditional_branch(is_nil.clone(), is_nil_block.clone(), is_not_nil_block);
        builder.position_at_end(is_nil_block.clone());
        builder.build_return(Some(&context.bool_type().const_int(0, false)));
        builder.position_at_end(is_not_nil_block.clone());
        let is_bool = builder.build_int_compare(
            inkwell::IntPredicate::EQ,
            cmp_tag.into_int_value(),
            context.i8_type().const_int(1, false),
            "is_bool",
        );
        let is_bool_block = context.append_basic_block(func.clone(), "is_bool");
        let is_not_bool_block = context.append_basic_block(func, "is_not_bool");
        builder.build_conditional_branch(is_bool, is_bool_block.clone(), is_not_bool_block.clone());
        builder.position_at_end(is_not_bool_block.clone());
        builder.build_return(Some(&context.bool_type().const_int(1, false)));
        builder.position_at_end(is_bool_block);
        let casted_t = builder.build_bitcast(
            arg_ptr,
            Self::tvalue_bool_type(context).ptr_type(0u16.into()),
            "t_bool",
        );
        let gep = unsafe {
            builder.build_in_bounds_gep(
                casted_t.get_type().ptr_type(0u16.into()),
                casted_t.into_pointer_value(),
                &[
                    context.i32_type().const_int(0, false),
                    context.i32_type().const_int(1, false),
                ],
                "raw_variant_pointer",
            )
        };
        let load_bool = builder.build_load(gep.get_type().ptr_type(0u16.into()), gep, "bool_t_int");
        let ret = builder.build_int_compare(
            inkwell::IntPredicate::EQ,
            load_bool.into_int_value(),
            context.i8_type().const_int(1, false),
            "ret",
        );
        builder.build_return(Some(&ret));
    }
}

pub struct TableField {
    pub name: String,
    pub value: TValue,
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
    pub fn push(&mut self, other: Self) {
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
