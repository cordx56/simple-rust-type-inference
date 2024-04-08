use std::collections::HashMap;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Name(String);
impl<S> From<S> for Name
where
    S: AsRef<str>,
{
    fn from(value: S) -> Self {
        Self(value.as_ref().to_owned())
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Param(String);
impl<S> From<S> for Param
where
    S: AsRef<str>,
{
    fn from(value: S) -> Self {
        Self(value.as_ref().to_owned())
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Type {
    Type(Vec<Type>),
    Param(Param),
    Inferred,
}
impl Type {
    pub fn ty<P>(params: P) -> Self
    where
        P: IntoIterator<Item = Type>,
    {
        let params = params.into_iter().collect();
        Self::Type(params)
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Function<'p, 't, 'r> {
    type_params: Vec<&'p Type>,
    param_types: Vec<&'t Type>,
    ret: &'r Type,
}
impl<'p, 't, 'r> Function<'p, 't, 'r> {
    pub fn new<P, T>(type_params: P, param_types: T, ret: &Type) -> Self
    where
        P: IntoIterator<Item = &'p Type>,
        T: IntoIterator<Item = &'t Type>,
    {
        let type_params = type_params.into_iter().collect();
        let param_types = param_types.into_iter().collect();
        Self {
            type_params,
            param_types,
            ret,
        }
    }
}

pub struct Definitions {
    types: HashMap<Name, Type>,
    funcs: HashMap<Name, Function>,
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
        N: Into<Name>,
    {
        self.types.get(&name.into())
    }
    pub fn func<N>(&self, name: N) -> Option<&Function>
    where
        N: Into<Name>,
    {
        self.funcs.get(&name.into())
    }
}

#[derive(Clone, Debug)]
pub struct Program(Vec<Statement>);
impl FromIterator<Statement> for Program {
    fn from_iter<T: IntoIterator<Item = Statement>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

#[derive(Clone, Debug)]
pub struct Statement {
    var: Name,
    ty: Option<Type>,
    expr: Expression,
}
impl Statement {
    pub fn new<N>(var: N, ty: Option<Type>, expr: Expression) -> Self
    where
        N: Into<Name>,
    {
        let var = var.into();
        let ty = ty.map(|t| t.into());
        Self { var, ty, expr }
    }
}

#[derive(Clone, Debug)]
pub enum Expression {
    Var(Name),
    Call { func: Name, params: Vec<Expression> },
}
impl Expression {
    pub fn var<N>(name: N) -> Self
    where
        N: Into<Name>,
    {
        Self::Var(name.into())
    }
    pub fn call<N, P>(func: N, params: P) -> Self
    where
        N: Into<Name>,
        P: IntoIterator<Item = Expression>,
    {
        let func = func.into();
        let params = params.into_iter().collect();
        Self::Call { func, params }
    }
}
