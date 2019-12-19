module Test/Data {
  data Fix[F] {
    case Unfix(f: F[Fix[F]])
  }
}
