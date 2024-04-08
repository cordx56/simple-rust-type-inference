mod ast;
mod infer;

fn main() {}

#[cfg(test)]
mod tests {
    use super::{ast::*, infer::*};

    #[test]
    fn infer_vec() {
        let vec_param = Param::from("E");
        let vec = Type::ty([vec_param]);
        let vec_new = Function::new([&vec_param], [], &vec);
        let program = Program::from_iter([
            Statement::new("num", None, Expression::call("i32", [])),
            Statement::new(
                "vec",
                None,
                Expression::call("vec_new", [Expression::var("num")]),
            ),
        ]);
    }
}
