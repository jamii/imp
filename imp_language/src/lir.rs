use crate::shared::*;

// Logical Intermediate Representation

// TODO instead of inlining maybe-infinite types, make a FunLir that takes a single set arg
// TODO can inline any ValueLir that is only used once
// TODO can inline any ValueLir that is used monotonically in only one other ValueLir

#[derive(Debug, Clone)]
pub struct ValueLir {
    pub name: Name,
    pub typ: ValueType,
    pub args: Vec<Name>,
    pub body: BooleanLir,
}

#[derive(Debug, Clone)]
pub enum BooleanLir {
    None,
    Some,
    Union(Box<BooleanLir>, Box<BooleanLir>),
    Intersect(Box<BooleanLir>, Box<BooleanLir>),
    ScalarEqual(ScalarRef, ScalarRef),
    Negate(Box<BooleanLir>),
    Apply(ValueRef, Vec<ScalarRef>),
}

#[derive(Debug, Clone)]
pub enum ValueRef {
    Name(Name),
    Native(Native),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ScalarRef {
    Name(Name),
    Scalar(Scalar),
}

#[derive(Debug, Clone)]
pub struct Lirs {
    pub lirs: Vec<ValueLir>,
}

#[derive(Debug)]
struct LirContext<'a> {
    lirs: RefCell<&'a mut Vec<ValueLir>>,
    gensym: &'a Gensym,
    type_cache: RefCell<&'a mut Cache<ValueType>>,
}

type Bindings<'a> = Environment<Binding<'a>>;

#[derive(Debug, Clone)]
enum Binding<'a> {
    Constant(bool),
    Abstract,
    LetFun { expr: &'a Expression },
    LetSet { name: Name, scope_names: Vec<Name> },
}

impl<'a> LirContext<'a> {
    fn define(
        &self,
        name: Option<Name>,
        typ: ValueType,
        args: Vec<Name>,
        body: BooleanLir,
    ) -> Name {
        let name = name.unwrap_or_else(|| self.gensym.name());
        let lir = ValueLir {
            name: name.clone(),
            typ,
            args,
            body: body.simplify(),
        };
        debug!("{}", lir);
        self.lirs.borrow_mut().push(lir);
        name
    }

    fn as_scalar_ref(&self, env: &Bindings, expr: &Expression) -> Option<ScalarRef> {
        if let Expression::Scalar(scalar) = expr {
            return Some(ScalarRef::Scalar(scalar.clone()));
        }
        if let Expression::Name(name) = expr {
            if let Binding::Abstract = env.lookup(name).unwrap() {
                return Some(ScalarRef::Name(name.clone()));
            }
        }
        None
    }
}

impl Expression {
    pub fn lirs(&self, type_cache: &mut Cache<ValueType>, gensym: &Gensym) -> Result<Lirs, String> {
        let body = self.with_unique_names(gensym, type_cache);
        let mut lirs = vec![];
        let arg_names = gensym.names(type_cache.get(self).arity().unwrap_or(0));
        let context = LirContext {
            lirs: RefCell::new(&mut lirs),
            gensym,
            type_cache: RefCell::new(type_cache),
        };
        let body = body.lir_into(&context, &Environment::new(), &BooleanLir::Some, &arg_names)?;
        let self_type = context.type_cache.borrow().get(self).clone();
        context.define(
            Option::Some("<main>".to_owned()),
            self_type,
            arg_names,
            body,
        );
        Ok(Lirs { lirs })
    }

    fn lir_into(
        &self,
        context: &LirContext,
        env: &Bindings,
        scope: &BooleanLir,
        arg_names: &[Name],
    ) -> Result<BooleanLir, String> {
        use BooleanLir as B;
        use Expression::*;
        let self_type = context.type_cache.borrow().get(self).clone();
        let self_arity = self_type.arity();
        debug!("self: {}", self);
        d!(arg_names);
        match self_arity {
            Option::None => return Ok(B::None),
            Option::Some(self_arity) => assert_eq!(self_arity, arg_names.len()),
        }
        let lir = match self {
            None => B::None,
            Some => B::Some,
            Scalar(scalar) => B::ScalarEqual(
                ScalarRef::Scalar(scalar.clone()),
                ScalarRef::Name(arg_names[0].clone()),
            ),
            Union(a, b) => {
                let a_lir = a.lir_into(context, env, scope, arg_names)?;
                let b_lir = if self_arity.unwrap() == 0 {
                    // can only reach b if !a
                    let scope = B::Intersect(box scope.clone(), box B::Negate(box a_lir.clone()));
                    b.lir_into(context, env, &scope, arg_names)?
                } else {
                    b.lir_into(context, env, scope, arg_names)?
                };
                B::Union(box a_lir, box b_lir)
            }
            Intersect(a, b) => {
                let a_lir = a.lir_into(context, env, scope, arg_names)?;
                // can only reach b if exists a
                let scope = B::Intersect(box scope.clone(), box a_lir.clone());
                let b_lir = b.lir_into(context, env, &scope, arg_names)?;
                B::Intersect(box a_lir, box b_lir)
            }
            Product(a, b) => {
                let a_arity = context.type_cache.borrow().get(a).arity().unwrap();
                let a_lir = a.lir_into(context, env, scope, &arg_names[..a_arity])?;
                // can only reach b if exists a
                let scope = B::Intersect(box scope.clone(), box a_lir.clone());
                let b_lir = b.lir_into(context, env, &scope, &arg_names[a_arity..])?;
                B::Intersect(box a_lir, box b_lir)
            }
            Equal(a, b) => {
                match (context.as_scalar_ref(env, a), context.as_scalar_ref(env, b)) {
                    (Option::Some(a_ref), Option::Some(b_ref)) => B::ScalarEqual(a_ref, b_ref),
                    _ => {
                        let ab_arity = context.type_cache.borrow().get(a).arity().unwrap_or(0);
                        let scope_names = a
                            .free_names()
                            .union(&b.free_names())
                            .cloned()
                            .filter(|name| {
                                if let Option::Some(Binding::Abstract) = env.lookup(name) {
                                    true
                                } else {
                                    false
                                }
                            })
                            .collect::<Vec<_>>();
                        let arg_names = context.gensym.names(ab_arity);
                        let self_names = scope_names
                            .iter()
                            .chain(arg_names.iter())
                            .cloned()
                            .collect::<Vec<_>>();
                        let a_name = context.define(
                            Option::None,
                            context.type_cache.borrow().get(a).clone(),
                            self_names.clone(),
                            B::Intersect(
                                box scope.clone(),
                                box a.lir_into(context, env, scope, &arg_names)?,
                            ),
                        );
                        let b_name = context.define(
                            Option::None,
                            context.type_cache.borrow().get(b).clone(),
                            self_names.clone(),
                            B::Intersect(
                                box scope.clone(),
                                box b.lir_into(context, env, scope, &arg_names)?,
                            ),
                        );

                        // !(((a n) & ! (b n)) | ((b n) & !(a n)))
                        let self_refs = self_names
                            .iter()
                            .map(|name| ScalarRef::Name(name.clone()))
                            .collect::<Vec<_>>();
                        B::Negate(box B::Union(
                            box B::Intersect(
                                box B::Apply(ValueRef::Name(a_name.clone()), self_refs.clone()),
                                box B::Negate(box B::Apply(
                                    ValueRef::Name(b_name.clone()),
                                    self_refs.clone(),
                                )),
                            ),
                            box B::Intersect(
                                box B::Apply(ValueRef::Name(b_name.clone()), self_refs.clone()),
                                box B::Negate(box B::Apply(
                                    ValueRef::Name(a_name.clone()),
                                    self_refs.clone(),
                                )),
                            ),
                        ))
                    }
                }
            }
            Negate(a) => {
                let a_arity = context.type_cache.borrow().get(a).arity().unwrap_or(0);
                let arg_names = context.gensym.names(a_arity);
                B::Negate(box a.lir_into(context, env, scope, &arg_names)?)
            }
            Name(name) => match env.lookup(name).unwrap() {
                Binding::Constant(b) => {
                    if *b {
                        B::Some
                    } else {
                        B::None
                    }
                }
                Binding::Abstract => B::ScalarEqual(
                    ScalarRef::Name(name.clone()),
                    ScalarRef::Name(arg_names[0].clone()),
                ),
                Binding::LetFun { expr } => {
                    let type_cache = &mut context.type_cache.borrow_mut();
                    expr.with_unique_names(context.gensym, type_cache)
                }
                .lir_into(context, env, scope, arg_names)?,
                Binding::LetSet { name, scope_names } => B::Apply(
                    ValueRef::Name(name.clone()),
                    scope_names
                        .iter()
                        .chain(arg_names.iter())
                        .map(|name| ScalarRef::Name(name.clone()))
                        .collect(),
                ),
            },
            Let(name, value, body) => {
                let mut env = env.clone();
                if context.type_cache.borrow().get(value).is_function() {
                    env.bind(name.clone(), Binding::LetFun { expr: value });
                } else {
                    let value_arg_names = context
                        .gensym
                        .names(context.type_cache.borrow().get(value).arity().unwrap_or(0));
                    let value_lir = value.lir_into(context, &env, scope, &value_arg_names)?;
                    if let B::None = value_lir {
                        env.bind(name.clone(), Binding::Constant(false));
                    } else if let B::Some = value_lir {
                        env.bind(name.clone(), Binding::Constant(true));
                    } else {
                        let scope_names = value
                            .free_names()
                            .into_iter()
                            .filter(|name| {
                                if let Option::Some(Binding::Abstract) = env.lookup(name) {
                                    true
                                } else {
                                    false
                                }
                            })
                            .collect::<Vec<_>>();
                        context.define(
                            Option::Some(name.clone()),
                            context.type_cache.borrow().get(value).clone(),
                            scope_names
                                .iter()
                                .chain(value_arg_names.iter())
                                .cloned()
                                .collect(),
                            B::Intersect(box scope.clone(), box value_lir),
                        );
                        env.bind(
                            name.clone(),
                            Binding::LetSet {
                                name: name.clone(),
                                scope_names: scope_names.to_vec(),
                            },
                        );
                    }
                }
                body.lir_into(context, &env, scope, arg_names)?
            }
            // TODO want to only calc cond once, but surprisingly tricky
            If(cond, if_true, if_false) => {
                let cond_lir = cond.lir_into(context, env, scope, &[])?;
                let if_true_lir = {
                    // can only reach if_true if cond
                    let scope = B::Intersect(box scope.clone(), box cond_lir.clone());
                    if_true.lir_into(context, env, &scope, arg_names)?
                };
                let if_false_lir = {
                    // can only reach if_false if !cond
                    let scope =
                        B::Intersect(box scope.clone(), box B::Negate(box cond_lir.clone()));
                    if_false.lir_into(context, env, &scope, arg_names)?
                };
                B::Union(
                    box B::Intersect(box cond_lir.clone(), box if_true_lir),
                    box B::Intersect(box B::Negate(box cond_lir), box if_false_lir),
                )
            }
            Abstract(name, body) => {
                let mut env = env.clone();
                env.bind(name.clone(), Binding::Abstract);
                let scope = B::Intersect(
                    box scope.clone(),
                    box B::ScalarEqual(
                        ScalarRef::Name(name.clone()),
                        ScalarRef::Name(arg_names[0].clone()),
                    ),
                );
                B::Intersect(
                    box B::ScalarEqual(
                        ScalarRef::Name(name.clone()),
                        ScalarRef::Name(arg_names[0].clone()),
                    ),
                    box body.lir_into(context, &env, &scope, &arg_names[1..])?,
                )
            }
            Apply(a, b) => {
                let mut a = &**a;
                let mut b = &**b;
                if context.type_cache.borrow().get(a).is_function() {
                    std::mem::swap(&mut a, &mut b);
                }
                let a_arity = context.type_cache.borrow().get(a).arity().unwrap_or(0);
                let b_arity = context.type_cache.borrow().get(b).arity().unwrap_or(0);
                let min_arity = a_arity.min(b_arity);
                let applied_arg_names = context.gensym.names(min_arity);
                let mut a_arg_names = applied_arg_names.clone();
                let mut b_arg_names = applied_arg_names.clone();
                if a_arity > b_arity {
                    a_arg_names.extend(arg_names.iter().cloned());
                } else {
                    b_arg_names.extend(arg_names.iter().cloned());
                }
                let a_lir = a.lir_into(context, env, scope, &a_arg_names)?;
                let b_lir = if context.type_cache.borrow().get(b).is_function() {
                    let scope = B::Intersect(box scope.clone(), box a_lir.clone());
                    b.lir_into(context, env, &scope, &b_arg_names)?
                } else {
                    b.lir_into(context, env, scope, &b_arg_names)?
                };
                B::Intersect(box a_lir, box b_lir)
            }
            Native(native) => B::Apply(
                ValueRef::Native(native.clone()),
                arg_names
                    .iter()
                    .map(|name| ScalarRef::Name(name.clone()))
                    .collect(),
            ),
            Solve(a) => {
                dbg!(arg_names);
                let mut a_lir = a.lir_into(context, env, scope, arg_names)?;
                d!(&a_lir);
                a_lir.resolve_eqs(arg_names);
                d!(&a_lir);
                let a_lir = a_lir.simplify();
                d!(&a_lir);
                ValueLir::validate_inner(arg_names, &a_lir)
                    .map_err(|err| format!("unsolvable: {}", err))?;
                a_lir
            }
            Reduce(..) | Seal(..) | Unseal(..) => panic!("Unimplemented: {:?}", self),
        };

        debug!("self: {}  =>  lir: {}", self, lir);

        Ok(lir)
    }
}

impl ValueLir {
    pub fn validate(&self) -> Result<(), String> {
        ValueLir::validate_inner(&self.args, &self.body)
    }

    fn validate_inner(args: &[Name], body: &BooleanLir) -> Result<(), String> {
        match body {
            BooleanLir::None => (),
            BooleanLir::Some => {
                if args.len() > 0 {
                    Err(format!("All args unbound - body is some"))?
                }
            }
            _ => {
                let mut bound = HashSet::new();
                body.validate(&mut bound)
                    .map_err(|s| format!("{} in {}", s, body))?;
                for arg in args {
                    if !bound.contains(arg) {
                        return Err(format!("arg {} not bound in {}", arg, body));
                    }
                }
            }
        }
        Ok(())
    }

    fn resolve_eqs(&self) -> Self {
        let mut body = self.body.clone();
        body.resolve_eqs(&self.args);
        ValueLir {
            name: self.name.clone(),
            typ: self.typ.clone(),
            args: self.args.clone(),
            body: body.simplify(),
        }
    }
}

impl BooleanLir {
    fn simplify(&self) -> BooleanLir {
        use BooleanLir::*;
        match self {
            None => None,
            Some => Some,
            Union(a, b) => match (a.simplify(), b.simplify()) {
                (None, c) | (c, None) => c,
                (Some, _) | (_, Some) => Some,
                (a, b) => Union(box a, box b),
            },
            Intersect(a, b) => match (a.simplify(), b.simplify()) {
                (None, _) | (_, None) => None,
                (Some, c) | (c, Some) => c,
                (a, b) => Intersect(box a, box b),
            },
            ScalarEqual(a, b) => match (a, b) {
                (ScalarRef::Scalar(a), ScalarRef::Scalar(b)) => {
                    if a == b {
                        Some
                    } else {
                        None
                    }
                }
                (ScalarRef::Name(a), ScalarRef::Name(b)) if a == b => Some,
                _ => ScalarEqual(a.clone(), b.clone()),
            },
            Negate(a) => match a.simplify() {
                None => Some,
                Some => None,
                a => Negate(box a),
            },
            Apply(f, args) => Apply(f.clone(), args.clone()),
        }
    }

    fn validate<'a>(&'a self, bound: &mut HashSet<&'a Name>) -> Result<(), String> {
        use BooleanLir::*;
        match self {
            None | Some => Err(format!("{} nested in lir", self))?,
            Union(a, b) => {
                let mut a_bound = bound.clone();
                let mut b_bound = bound.clone();
                a.validate(&mut a_bound)?;
                b.validate(&mut b_bound)?;
                *bound = a_bound.intersection(&b_bound).map(|n| &**n).collect();
            }
            Intersect(a, b) => {
                a.validate(bound)?;
                b.validate(bound)?;
            }
            ScalarEqual(a, b) => match (a, b) {
                (ScalarRef::Scalar(_), ScalarRef::Scalar(_)) => (),
                (ScalarRef::Name(name), ScalarRef::Scalar(_))
                | (ScalarRef::Scalar(_), ScalarRef::Name(name)) => {
                    bound.insert(name);
                }
                (ScalarRef::Name(name_a), ScalarRef::Name(name_b)) => {
                    if bound.contains(name_a) {
                        bound.insert(name_b);
                    } else if bound.contains(name_b) {
                        bound.insert(name_a);
                    } else {
                        return Err(format!("{}={} but neither bound yet", name_a, name_b));
                    }
                }
            },
            Negate(_) => (),
            Apply(v, args) => match v {
                ValueRef::Name(_) => {
                    for arg in args {
                        if let ScalarRef::Name(name) = arg {
                            bound.insert(name);
                        }
                    }
                }
                ValueRef::Native(native) => {
                    if args[0..native.input_arity].iter().all(|arg| match arg {
                        ScalarRef::Scalar(_) => true,
                        ScalarRef::Name(name) => bound.contains(name),
                    }) {
                        for arg in &args[native.input_arity..] {
                            if let ScalarRef::Name(name) = arg {
                                bound.insert(name);
                            }
                        }
                    } else {
                        return Err(format!("{} but input not bound yet", v));
                    }
                }
            },
        }
        Ok(())
    }

    fn resolve_eqs(&mut self, args: &[Name]) {
        use BooleanLir::*;

        let mut eqs: Vec<(ScalarRef, ScalarRef)> = vec![];
        {
            let mut stack = vec![&mut *self];
            while let Option::Some(lir) = stack.pop() {
                match lir {
                    Intersect(a, b) => {
                        stack.push(a);
                        stack.push(b);
                    }
                    ScalarEqual(a, b) => {
                        eqs.push((a.clone(), b.clone()));
                    }
                    _ => (),
                }
            }
        }

        let mut groups: Vec<HashSet<ScalarRef>> = vec![];
        {
            for (a, b) in eqs {
                let a_ix = groups.iter().position(|group| group.contains(&a));
                let b_ix = groups.iter().position(|group| group.contains(&b));
                match (a_ix, b_ix) {
                    (Option::Some(a_ix), Option::Some(b_ix)) => {
                        if a_ix != b_ix {
                            if a_ix < b_ix {
                                let b_group = groups.swap_remove(b_ix);
                                groups[a_ix].extend(b_group);
                            } else {
                                let a_group = groups.swap_remove(a_ix);
                                groups[b_ix].extend(a_group);
                            }
                        }
                    }
                    (Option::Some(a_ix), Option::None) => {
                        groups[a_ix].insert(b);
                    }
                    (Option::None, Option::Some(b_ix)) => {
                        groups[b_ix].insert(a);
                    }
                    (Option::None, Option::None) => {
                        groups.push(HashSet::from_iter(vec![a, b]));
                    }
                }
            }
        }
        d!(&groups);

        let mut renames: HashMap<ScalarRef, ScalarRef> = HashMap::new();
        let mut keeps: Vec<(ScalarRef, ScalarRef)> = vec![];
        {
            for group in groups {
                let first_scalar = group.iter().find(|sref| match sref {
                    ScalarRef::Name(_) => false,
                    ScalarRef::Scalar(_) => true,
                });
                let first_arg = group.iter().find(|sref| match sref {
                    ScalarRef::Name(name) => args.iter().any(|arg| arg == name),
                    ScalarRef::Scalar(_) => false,
                });
                let target = first_scalar
                    .unwrap_or(first_arg.unwrap_or(group.iter().next().unwrap()))
                    .clone();
                for sref in group {
                    match &sref {
                        ScalarRef::Name(name) => {
                            if args.iter().any(|arg| arg == name) {
                                // need to remember the value to be able to return it from the ValueLir :(
                                keeps.push((sref.clone(), target.clone()))
                            }
                            renames.insert(sref, target.clone());
                        }
                        ScalarRef::Scalar(scalar) => {
                            if let ScalarRef::Scalar(scalar2) = &target {
                                if scalar != scalar2 {
                                    // unsatisfiable, bail out
                                    *self = None;
                                    return;
                                }
                            }
                        }
                    }
                }
            }
        }
        d!(&renames);

        {
            let mut stack = vec![&mut *self];
            while let Option::Some(lir) = stack.pop() {
                match lir {
                    Intersect(a, b) => {
                        stack.push(a);
                        stack.push(b);
                    }
                    Union(a, b) => {
                        stack.push(a);
                        stack.push(b);
                    }
                    Negate(a) => {
                        stack.push(a);
                    }
                    ScalarEqual(a, b) => {
                        if let Option::Some(new_a) = renames.get(a) {
                            *a = new_a.clone();
                        }
                        if let Option::Some(new_b) = renames.get(b) {
                            *b = new_b.clone();
                        }
                    }
                    Apply(_, args) => {
                        for arg in args {
                            if let Option::Some(new_arg) = renames.get(arg) {
                                *arg = new_arg.clone();
                            }
                        }
                    }
                    _ => (),
                }
            }
        }

        {
            let mut stack = vec![&mut *self];
            while let Option::Some(lir) = stack.pop() {
                match lir {
                    Intersect(a, b) => {
                        stack.push(a);
                        stack.push(b);
                    }
                    Union(a, b) => {
                        a.resolve_eqs(args);
                        b.resolve_eqs(args);
                    }
                    Negate(a) => {
                        a.resolve_eqs(args);
                    }
                    _ => (),
                }
            }
        }

        for (a, b) in keeps {
            *self = Intersect(box std::mem::replace(self, None), box ScalarEqual(a, b))
        }
    }
}

impl Lirs {
    pub fn resolve_eqs(&self) -> Self {
        Lirs {
            lirs: self.lirs.iter().map(|lir| lir.resolve_eqs()).collect(),
        }
    }
}
