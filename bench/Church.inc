module Test/Church {
    let zero = a -> a
    let one = f -> a -> f(a)
    let two = f -> a -> f(f(a))
    let three = f -> a -> f(f(f(a)))
    let plus = m -> n -> f -> x -> m(f)(n(f)(x))
    let succ = n -> f -> x -> f(n(f(x)))
    let mult = m -> n -> f -> x -> m(n(f))(x)
    let five = f -> a -> plus(two)(three)(f)(a)
    let ten = f -> a -> mult(two)(five)(f)(a)
    let exp = m -> n -> n(m)
    let tru = a -> b -> a
    let fals = a -> b -> b
}
