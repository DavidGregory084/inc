package inc.common

trait Substitutable[A, B, C] {
  def substitute(c: C, subst: Substitution[A, B]): C
}
