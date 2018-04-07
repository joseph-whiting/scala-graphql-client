object WithName {
  implicit def innerObject[T](outer: TypedWithName[T]): T = outer.inner
  def ::[T](typed: T) = new TypedWithName[T](typed)
  class TypedWithName[T] (val inner: T) extends Name
}

object WithAge {
  implicit def innerObject[T](outer: TypedWithAge[T]): T = outer.inner
  def ::[T](typed: T) = new TypedWithAge[T](typed)
  class TypedWithAge[T] (val inner: T) extends Age
}

class Character {}


trait Field

trait Name extends Field {
  var name: String = "Luke"
}
trait Age extends Field {
  var age: Integer = 23
}

object Luke extends Character {}

object Test {
  val lukeWithName = Luke :: WithName
  lukeWithName.name = "Han"
  val lukesName = lukeWithName.name

  val lukeWithAge = Luke :: WithAge
  lukeWithAge.age = 22
  val lukesAge = lukeWithAge.age

  val lukeWithAgeAndName = (Luke :: WithAge) :: WithName

  lukeWithAgeAndName.age = 28
  val age2 = lukeWithAgeAndName.age
  val name2 = lukeWithAgeAndName.name
}
