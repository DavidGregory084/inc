module Test/Data {
  data Either[A, B] {
    case Left(a: A)
    case Right(b: B)
  }
}
