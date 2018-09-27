package hayago

final case class IdOps[A](self: A) {
  def ??(d: => A)/*(implicit ev: Null <:< A)*/: A = if (self == null) d else self
  def |>[B](f: A => B): B = f(self)
  def <|(f: A => Any): A = { f (self); self }
  def squared: (A, A) = (self, self)
}