module Test/Data {
  data Fix[F] {
    case Fix(unfix: F[Fix[F]])
  }
}
