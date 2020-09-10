module Test/Data/NonEmptyList {
  data List[A] {
    case Cons(head: A, tail: List[A])
    case Nil()
  }

  data NonEmptyList[A] {
    case NonEmpty(head: A, tail: List[A])
  }

  let first = nel -> match nel with {
    case NonEmpty { head } -> head
  }

  let rest = nel -> match nel with {
    case NonEmpty { tail } -> tail
  }
}
