module Test/Data {
  data List[A] {
    case Cons(head: A, tail: List[A])
    case Nil()
  }
}
