module Test/Data {
  data Option[A] {
    case Some(a: A)
    case None()
  }
}
