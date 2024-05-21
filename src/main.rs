mod ast;
mod infer;

use ast::*;
use infer::*;

fn main() {
    let mut env = VarEnv::new();
    env.insert("int", Type::func([], Type::ty("i32", [])));
    env.insert("new", Type::func([], Type::ty("Vec", [Type::param("T")])));
    env.insert(
        "push",
        Type::func(
            [Type::ty("Vec", [Type::param("T")]), Type::param("T")],
            Type::ty("()", []),
        ),
    );
    let int_type = Type::ty("i32", []);
    let int_new = Function::new("int", [], [], int_type);
    let vec_type = Type::ty("Vec", [Type::param("T")]);
    let vec_new = Function::new("Vec::new", [], [], vec_type);
    let num = Var::from("num");
    let vec = Var::from("vec");
    let res = Var::from("ret");
    let program = Program::from_iter([
        Statement::new(vec.clone(), Expression::call("new", [])),
        Statement::new(num.clone(), Expression::call("int", [])),
        Statement::new(
            res,
            Expression::call("push", [Expression::var(vec), Expression::var(num)]),
        ),
    ]);
    let res = env.analyze(&program);
    println!("{:?}", res);
    env.print();
}
