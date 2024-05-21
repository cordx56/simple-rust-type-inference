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
impl TypeParam {
    pub fn from<S>(value: S) -> Self
    where
        S: ToString,
    {
        Self(value.to_string())
    }
}
impl std::fmt::Display for TypeParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Type<P = TypeParam> {
    Type(TypeName, Vec<Type<P>>),
    Func(Vec<Type<P>>, Box<Type<P>>),
    Param(P),
    Inferred,
}
impl<P> Type<P> {
    pub fn ty<N, Q>(name: N, params: Q) -> Self
    where
        N: Into<TypeName>,
        Q: IntoIterator<Item = Type<P>>,
    {
        let params = params.into_iter().collect();
        Self::Type(name.into(), params)
    }
    pub fn func<A>(args: A, ret: Self) -> Self
    where
        A: IntoIterator<Item = Type<P>>,
    {
        Self::Func(args.into_iter().collect(), Box::new(ret))
    }
}
impl Type<TypeParam> {
    pub fn param<S>(param: S) -> Self
    where
        S: ToString,
    {
        Self::Param(TypeParam::from(param))
    }
}
impl<P> std::fmt::Display for Type<P>
where
    P: std::fmt::Display,
{
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
            Type::Param(param) => write!(f, "{}", param),
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
    pub fn new<V>(var: V, expr: Expression) -> Self
    where
        V: Into<Var>,
    {
        Self {
            var: var.into(),
            expr,
        }
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
