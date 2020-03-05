module Test/Data/Fix {
  data Fix[F] {
    case Unfix(unfix: F[Fix[F]])
  }
}
