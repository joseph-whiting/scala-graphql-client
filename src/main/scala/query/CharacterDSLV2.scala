class WithField[F <: Field] {
  implicit def innerObject[T](outer: TypedWithField[T]): T = outer.inner
  def ::[T](typed: T) = new TypedWithField[T](typed)
  class TypedWithField[T] (val inner: T) extends Name
}

class Character {}

abstract trait Field
trait Name extends Field {
  var name: String = "Luke"
}

object WithName extends WithField[Name] {}

object Luke extends Character {}

object Test {
  val lukeWithName = (Luke :: WithName)
  lukeWithName.name = "Han"
  val lukesName = lukeWithName.name
}
