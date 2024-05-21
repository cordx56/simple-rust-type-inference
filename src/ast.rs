use std::collections::HashMap;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct TypeName(pub String);
impl<S> From<S> for TypeName
where
    S: ToString,
{
    fn from(value: S) -> Self {
        Self(value.to_string())
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct TypeParam(pub String);
impl<S> From<S> for TypeParam
where
    S: ToString,
{
    fn from(value: S) -> Self {
        Self(value.to_string())
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Type<R = TypeParam> {
    Type(TypeName, Vec<Type<R>>),
    Func(Vec<Type<R>>, Box<Type<R>>),
    Param(R),
    Inferred,
}
impl Type<TypeParam> {
    pub fn ty<N, P>(name: N, params: P) -> Self
    where
        N: Into<TypeName>,
        P: IntoIterator<Item = Type>,
    {
        let params = params.into_iter().collect();
        Self::Type(name.into(), params)
    }
    pub fn func<A>(args: A, ret: Self) -> Self
    where
        A: IntoIterator<Item = Type>,
    {
        Self::Func(args.into_iter().collect(), Box::new(ret))
    }
    pub fn param<P>(param: P) -> Self
    where
        P: Into<TypeParam>,
    {
        Self::Param(param.into())
    }
}
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Type(name, params) => {
                write!(f, "{}", name.0)?;
                let mut iter = params.iter();
                if let Some(ty) = iter.next() {
                    write!(f, "<{}", ty)?;
                    for ty in iter {
                        write!(f, ", {}", ty)?;
                    }
                    write!(f, ">")?;
                }
                Ok(())
            }
            Type::Func(args, ret) => {
                write!(f, "(")?;
                let mut iter = args.iter();
                if let Some(ty) = iter.next() {
                    write!(f, "{}", ty)?;
                    for ty in iter {
                        write!(f, ", {}", ty)?;
                    }
                }
                write!(f, ") -> {}", ret)?;
                Ok(())
            }
            Type::Param(param) => write!(f, "{}", param.0),
            Type::Inferred => {
                write!(f, "_")
            }
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Var(pub String);
impl<S> From<S> for Var
where
    S: ToString,
{
    fn from(value: S) -> Self {
        Self(value.to_string())
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Function {
    name: Var,
    type_params: Vec<TypeParam>,
    param_types: Vec<Type>,
    ret: Type,
}
impl Function {
    pub fn new<N, P, T>(name: N, type_params: P, param_types: T, ret: Type) -> Self
    where
        N: Into<Var>,
        P: IntoIterator<Item = TypeParam>,
        T: IntoIterator<Item = Type>,
    {
        let name = name.into();
        let type_params = type_params.into_iter().collect();
        let param_types = param_types.into_iter().collect();
        Self {
            name,
            type_params,
            param_types,
            ret,
        }
    }
}

pub struct Definitions {
    types: HashMap<TypeName, Type>,
    funcs: HashMap<Var, Function>,
}
impl Definitions {
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
            funcs: HashMap::new(),
        }
    }

    pub fn ty<N>(&self, name: N) -> Option<&Type>
    where
        N: Into<TypeName>,
    {
        self.types.get(&name.into())
    }
    pub fn func<N>(&self, name: N) -> Option<&Function>
    where
        N: Into<Var>,
    {
        self.funcs.get(&name.into())
    }
}

#[derive(Clone, Debug)]
pub struct Program(pub Vec<Statement>);
impl FromIterator<Statement> for Program {
    fn from_iter<T: IntoIterator<Item = Statement>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

#[derive(Clone, Debug)]
pub struct Statement {
    pub var: Var,
    pub expr: Expression,
}
impl Statement {
    pub fn new(var: Var, expr: Expression) -> Self {
        Self { var, expr }
    }
}

#[derive(Clone, Debug)]
pub enum Expression {
    Var(Var),
    Call { func: Var, params: Vec<Expression> },
}
impl Expression {
    pub fn var<N>(var: N) -> Self
    where
        N: Into<Var>,
    {
        Self::Var(var.into())
    }
    pub fn call<N, P>(func: N, params: P) -> Self
    where
        N: Into<Var>,
        P: IntoIterator<Item = Expression>,
    {
        let func = func.into();
        let params = params.into_iter().collect();
        Self::Call { func, params }
    }
}
