module Test/Data/Option {
  data Option[A] {
    case Some(a: A)
    case None()
  }
}
