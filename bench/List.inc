module Test/Data/List {
  data List[A] {
    case Cons(head: A, tail: List[A])
    case Nil()
  }
}
