module Test/Data/Tree {
  data Tree[A] {
    case Branch(l: Tree[A], r: Tree[A])
    case Leaf(a: A)
  }
}
