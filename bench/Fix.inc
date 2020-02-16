module Test/Foo/Data {
  data Fix[F] {
    case Unfix(unfix: F[Fix[F]])
  }
}
