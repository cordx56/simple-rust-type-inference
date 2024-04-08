use crate::ast::*;

pub fn unify(ty1: &Type, ty2: &Type) -> Type {
    match ty1 {
        Type::Name { id, params } => {}
        Type::Param(param) => {}
        Type::Inferred => {}
    };
}
