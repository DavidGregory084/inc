module Test/Const {
    let constant = (a, b) -> a
    let str = a -> "a"
    let app = constant(str(constant(32, 43)), "b")
}
