module Test/Const {
    let const = (a, b) -> a
    let str = a -> "a"
    let app = const(str(const(32, 43)), "a")
}
