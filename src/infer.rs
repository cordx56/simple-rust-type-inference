use crate::ast::*;

use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct UnifyEnv(HashMap<TypeParam, Type>);
impl UnifyEnv {
    pub fn new() -> Self {
        Self(HashMap::new())
    }
    pub fn resolve<'a>(&'a self, ty: &'a Type) -> &'a Type {
        if let Type::Param(p) = ty {
            if let Some(res) = self.0.get(p) {
                return self.resolve(res);
            }
        }
        ty
    }
    pub fn unify(&mut self, ty1: &Type, ty2: &Type) -> Result<Type, String> {
        let ty1 = self.resolve(ty1).clone();
        let ty2 = self.resolve(ty2).clone();
        match (&ty1, &ty2) {
            (Type::Param(p1), Type::Param(p2)) if p1 == p2 => Ok(ty2),
            (Type::Param(p), _) => {
                self.0.insert(p.clone(), ty2.clone());
                println!("{:?}", self);
                Ok(ty2)
            }
            (_, Type::Param(_)) => self.unify(&ty2, &ty1),
            (Type::Type(n1, p1), Type::Type(n2, p2)) => {
                if n1 != n2 || p1.len() != p2.len() {
                    return Err("unification failed".to_owned());
                }
                let p = p1
                    .iter()
                    .zip(p2.iter())
                    .map(|(p1, p2)| self.unify(p1, p2))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Type::Type(n1.clone(), p))
            }
            (Type::Func(a1, r1), Type::Func(a2, r2)) => {
                if a1.len() != a2.len() {
                    return Err("unification failed".to_owned());
                }
                let a = a1
                    .iter()
                    .zip(a2.iter())
                    .map(|(a1, a2)| self.unify(a1, a2))
                    .collect::<Result<Vec<_>, _>>()?;
                let r = self.unify(r1, r2)?;
                Ok(Type::Func(a, Box::new(r)))
            }
            (Type::Inferred, _) => Ok(ty2),
            (_, Type::Inferred) => Ok(ty1),
            _ => Err("unification failed".to_owned()),
        }
    }
}

#[derive(Clone, Debug)]
pub struct VarEnv {
    var: HashMap<Var, Type<usize>>,
    type_params: Vec<Type>,
}
impl VarEnv {
    pub fn new() -> Self {
        Self {
            var: HashMap::new(),
            type_params: Vec::new(),
        }
    }
    pub fn get<N>(&self, name: N) -> Option<&Type<usize>>
    where
        N: Into<Var>,
    {
        self.var.get(&name.into())
    }
    fn index_type_param(&mut self, params: &mut Vec<TypeParam>, ty: Type) -> Type<usize> {
        match ty {
            Type::Type(n, p) => Type::Type(
                n,
                p.into_iter()
                    .map(|p| self.index_type_param(params, p))
                    .collect(),
            ),
            Type::Func(a, r) => Type::Func(
                a.into_iter()
                    .map(|a| self.index_type_param(params, a))
                    .collect(),
                Box::new(self.index_type_param(params, *r)),
            ),
            Type::Param(p) => {
                if let Some(index) = params.iter().position(|pp| p == *pp) {
                    Type::Param(self.type_params.len() + index)
                } else {
                    let index = params.len();
                    params.push(p);
                    Type::Param(self.type_params.len() + index)
                }
            }
            Type::Inferred => Type::Inferred,
        }
    }
    pub fn insert<N>(&mut self, name: N, ty: Type)
    where
        N: Into<Var>,
    {
        let mut vec = Vec::new();
        let res = self.index_type_param(&mut vec, ty);
        self.type_params.extend_from_slice(vec.as_slice());
        self.var.insert(name.into(), res);
    }
    pub fn infer(&mut self, ue: &mut UnifyEnv, e: &Expression) -> Result<Type, String> {
        match e {
            Expression::Var(var) => self
                .0
                .get(var)
                .clone()
                .ok_or("variable not found".to_owned())
                .map(|ty| ue.resolve(&ty))
                .cloned(),
            Expression::Call { func, params } => {
                let f = self.var.get(func).cloned();
                if let Some(f) = f {
                    let args = params
                        .iter()
                        .map(|e| self.infer(ue, e))
                        .collect::<Result<Vec<_>, _>>()?;
                    let ty = ue.unify(&f, &Type::Func(args, Box::new(Type::Inferred)))?;
                    if let Type::Func(_, ret) = ty {
                        Ok(*ret.clone())
                    } else {
                        Err("invalid function".to_owned())
                    }
                } else {
                    Err("function not found".to_owned())
                }
            }
        }
    }
    pub fn statement(&mut self, stmt: &Statement) -> Result<(), String> {
        println!("stmt");
        let mut ue = UnifyEnv::new();
        let ty = self.infer(&mut ue, &stmt.expr)?;
        println!("ue: {:?}", ue);
        println!("{}", ty);
        self.0.insert(stmt.var.clone(), ty);
        Ok(())
    }
    pub fn analyze(&mut self, prog: &Program) -> Result<(), String> {
        for p in &prog.0 {
            self.statement(p)?;
        }
        Ok(())
    }
    pub fn print(&self) {
        for (k, v) in self.0.iter() {
            println!("{}: {}", k.0, v);
        }
    }
}
