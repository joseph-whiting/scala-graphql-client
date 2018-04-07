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

  object Client {
    def send[A](query: Query[A]) = query.parseResponse("")
  }

  abstract class AbstractQuery[A] {
    def generateQuery(): String
    def parseResponse(response: String): A
  }
  abstract class AbstractRootQuery[A] extends AbstractQuery[A] {
    def character[B](inner: () => B): AbstractQuery[B]
  }
  class Query[A](inner: AbstractQuery[A]) extends AbstractQuery[A] {
    def generateQuery() = inner.generateQuery()
    def parseResponse(response: String) = inner.parseResponse(response)
  }

  class CharacterQuery[B, C](field1: EmptyQuery => AbstractQuery[B], field2: AbstractQuery[B] => AbstractQuery[C]) extends AbstractQuery[C] {
    def generateQuery() = ""
    def parseResponse(response: String): C = field2(field1(new EmptyQuery())).parseResponse(response)
  }

  class NameField[A](inner: AbstractQuery[A]) extends AbstractQuery[WithName.TypedWithName[A]] {
    def generateQuery() = "name"
    def parseResponse(response: String) = inner.parseResponse(response) :: WithName // search for name in keys
  }

  class AgeField[A](inner: AbstractQuery[A]) extends AbstractQuery[WithAge.TypedWithAge[A]] {
    def generateQuery() = "age"
    def parseResponse(response: String) = inner.parseResponse(response) :: WithAge // search for age in keys
  }

  class EmptyQuery extends AbstractQuery[Character] {
    def generateQuery() = ""
    def parseResponse(response: String) = new Character()
  }

  def query[A](inner: AbstractQuery[A]) = new Query(inner)
  def character[A, B](field1: EmptyQuery => AbstractQuery[A], field2: AbstractQuery[A] => AbstractQuery[B]) = new CharacterQuery(field1, field2)
  def name(query: EmptyQuery) = new NameField(query)
  def age[A](query: AbstractQuery[A]) = new AgeField(query)

  Client.send(query {
                character(name, age[WithName.TypedWithName[Character]])
              }).name
}
