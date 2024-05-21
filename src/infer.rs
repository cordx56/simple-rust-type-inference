use crate::ast::*;

use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct UnifyEnv(HashMap<usize, Type<usize>>);
impl UnifyEnv {
    pub fn new() -> Self {
        Self(HashMap::new())
    }
    pub fn resolve<'a>(&self, ty: &Type<usize>) -> Type<usize> {
        match ty {
            Type::Type(n, p) => {
                Type::Type(n.clone(), p.into_iter().map(|p| self.resolve(p)).collect())
            }
            Type::Func(a, r) => Type::Func(
                a.into_iter().map(|a| self.resolve(a)).collect(),
                Box::new(self.resolve(r)),
            ),
            Type::Param(p) => {
                if let Some(res) = self.0.get(&p) {
                    self.resolve(res)
                } else {
                    ty.clone()
                }
            }
            Type::Inferred => Type::Inferred,
        }
    }
    pub fn unify(&mut self, ty1: &Type<usize>, ty2: &Type<usize>) -> Result<Type<usize>, String> {
        let ty1 = self.resolve(ty1).clone();
        let ty2 = self.resolve(ty2).clone();
        match (&ty1, &ty2) {
            (Type::Param(p1), Type::Param(p2)) if p1 == p2 => Ok(ty2),
            (Type::Param(p), _) => {
                self.0.insert(*p, ty2.clone());
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
                let r = self.unify(&r1, &r2)?;
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
    ue: UnifyEnv,
    var_id: usize,
}
impl VarEnv {
    pub fn new() -> Self {
        Self {
            var: HashMap::new(),
            ue: UnifyEnv::new(),
            var_id: 0,
        }
    }
    fn new_type_var(&mut self) -> usize {
        let index = self.var_id;
        self.var_id += 1;
        index
    }
    fn index_type_param(&mut self, map: &mut HashMap<TypeParam, usize>, ty: Type) -> Type<usize> {
        match ty {
            Type::Type(n, p) => Type::Type(
                n.clone(),
                p.into_iter()
                    .map(|p| self.index_type_param(map, p))
                    .collect(),
            ),
            Type::Func(a, r) => Type::Func(
                a.into_iter()
                    .map(|a| self.index_type_param(map, a))
                    .collect(),
                Box::new(self.index_type_param(map, *r)),
            ),
            Type::Param(ref p) => {
                if let Some(index) = map.get(&p) {
                    Type::Param(*index)
                } else {
                    let index = self.new_type_var();
                    map.insert(p.clone(), index);
                    Type::Param(index)
                }
            }
            Type::Inferred => Type::Inferred,
        }
    }
    pub fn insert<N>(&mut self, name: N, ty: Type)
    where
        N: Into<Var>,
    {
        let res = self.index_type_param(&mut HashMap::new(), ty);
        self.var.insert(name.into(), res);
    }
    pub fn clone_param(&mut self, map: &mut HashMap<usize, usize>, ty: Type<usize>) -> Type<usize> {
        match ty {
            Type::Type(n, p) => {
                Type::Type(n, p.into_iter().map(|p| self.clone_param(map, p)).collect())
            }
            Type::Func(a, r) => Type::Func(
                a.into_iter().map(|a| self.clone_param(map, a)).collect(),
                Box::new(self.clone_param(map, *r)),
            ),
            Type::Param(p) => {
                if let Some(index) = map.get(&p) {
                    Type::Param(*index)
                } else {
                    let new = self.new_type_var();
                    map.insert(p, new);
                    Type::Param(new)
                }
            }
            Type::Inferred => Type::Inferred,
        }
    }
    pub fn infer(&mut self, e: &Expression) -> Result<Type<usize>, String> {
        match e {
            Expression::Var(var) => self
                .var
                .get(var)
                .clone()
                .ok_or("variable not found".to_owned())
                //.map(|ty| ue.resolve(&ty))
                .cloned(),
            Expression::Call { func, params } => {
                let f = self.var.get(func).cloned();
                if let Some(f) = f {
                    let f = self.clone_param(&mut HashMap::new(), f);
                    let args = params
                        .iter()
                        .map(|e| self.infer(e))
                        .collect::<Result<Vec<_>, _>>()?;
                    let ty = self
                        .ue
                        .unify(&f, &Type::Func(args, Box::new(Type::Inferred)))?;
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
        let ty = self.infer(&stmt.expr)?;
        self.var.insert(stmt.var.clone(), self.ue.resolve(&ty));
        Ok(())
    }
    pub fn analyze(&mut self, prog: &Program) -> Result<(), String> {
        for p in &prog.0 {
            self.statement(p)?;
        }
        Ok(())
    }
    pub fn print(&self) {
        for (k, v) in self.var.iter() {
            println!("{}: {}", k.0, self.ue.resolve(v));
        }
    }
}
