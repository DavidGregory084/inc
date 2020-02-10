module Test/Data {
  data Fix[F] {
    case Unfix(unfix: F[Fix[F]])
  }
}
