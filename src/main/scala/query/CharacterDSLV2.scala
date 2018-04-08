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

class WithCharacters[A] {
  implicit def innerObject[B](outer: TypedWithCharacters[A, B]): B = outer.inner
  def ::[B](typed: B) = new TypedWithCharacters[A, B](typed)
}
class TypedWithCharacters[A, B] (val inner: B) extends Characters[A]
class EmptyType {}

trait Characters[A] {
  var characters: Seq[A] = Seq()
}

trait Name {
  var name: String = "Luke"
}
trait Age {
  var age: Integer = 23
}

object Test {
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

  class CharacterQuery[A, B](fields: AbstractQuery[A])(inner: AbstractQuery[B]) extends AbstractQuery[TypedWithCharacters[A, B]] {
    def generateQuery() = ""
    def parseResponse(response: String) = inner.parseResponse(response) :: new WithCharacters[A]()
  }

  class NameField[A](inner: AbstractQuery[A]) extends AbstractQuery[WithName.TypedWithName[A]] {
    def generateQuery() = "name"
    def parseResponse(response: String) = inner.parseResponse(response) :: WithName // search for name in keys
    def age() = new AgeField(this)
  }

  class AgeField[A](nextField: AbstractQuery[A]) extends AbstractQuery[WithAge.TypedWithAge[A]] {
    def generateQuery() = "age"
    def parseResponse(response: String) = nextField.parseResponse(response) :: WithAge // search for age in keys
  }

  class EmptyQuery extends AbstractQuery[EmptyType] {
    def generateQuery() = ""
    def parseResponse(response: String) = new EmptyType()
  }

  def query[A](inner: AbstractQuery[A]) = new Query(inner)
  def character[A]()(fields: AbstractQuery[A]) = new CharacterQuery(fields)(new EmptyQuery())
  def name: NameField[EmptyType] = new NameField(new EmptyQuery())

  Client.send(query {
                character()(
                  name
                  age
                )
              }).characters(2).age
}

