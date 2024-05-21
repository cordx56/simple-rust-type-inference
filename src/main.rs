mod ast;
mod infer;

use ast::*;
use infer::*;

fn main() {
    let mut env = VarEnv::new();
    env.insert("int", Type::func([], Type::ty("i32", [])));
    env.insert("float", Type::func([], Type::ty("f32", [])));
    env.insert("new", Type::func([], Type::ty("Vec", [Type::param("T")])));
    env.insert(
        "push",
        Type::func(
            [Type::ty("Vec", [Type::param("T")]), Type::param("T")],
            Type::ty("()", []),
        ),
    );
    let program = Program::from_iter([
        Statement::new("vec1", Expression::call("new", [])),
        Statement::new("i", Expression::call("int", [])),
        Statement::new(
            "ret",
            Expression::call("push", [Expression::var("vec1"), Expression::var("i")]),
        ),
        Statement::new("vec2", Expression::call("new", [])),
        Statement::new("f", Expression::call("float", [])),
        Statement::new(
            "ret",
            Expression::call("push", [Expression::var("vec2"), Expression::var("f")]),
        ),
    ]);
    let res = env.analyze(&program);
    println!("{:?}", res);
    if res.is_ok() {
        env.print();
    }
}
