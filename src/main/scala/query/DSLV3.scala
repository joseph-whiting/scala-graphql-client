import scala.concurrent._
import ExecutionContext.Implicits.global
object Client {
  def send[A](query: Query.Query[A]) = Future {
    query.parseResponse("")
  }

  object Character {
    class With[TFieldData] {
      def ::[TInner](typed: TInner) = new TypedWith[TInner, TFieldData](typed)
    }
    class Field[TParent, TField] extends AbstractField[TParent, TypedWith[TParent, TField]] {
      def name = "character"
      def addTrait(innerParseResult: TParent) = innerParseResult :: new With[TField]()
    }
    trait FieldTrait[TFieldData] {
      var character: TFieldData = _
    }
    class TypedWith[TInner, TFieldData](val inner: TInner) extends FieldTrait[TFieldData]
    implicit def innerObject[TInner, TFieldData](outer: TypedWith[TInner, TFieldData]): TInner = outer.inner
    class QueryWith[TResponse, TFieldData](val inner: AbstractQuery[TResponse])(val field: Field[TResponse, TFieldData]) extends FieldQuery(inner)(field)

    class CharacterFieldQuery[TResponse, TWrapper](innerQuery: AbstractQuery[TResponse])(field: AbstractField[TResponse, TWrapper]) extends FieldQuery[TResponse, TWrapper](innerQuery)(field) {
      def age() = new CharacterFieldQuery(this)(new Age.Field[TWrapper, Int])
    }
  }

  def character[TFieldData]()(subQuery: Character.CharacterFieldQuery[EmptyType, TFieldData]): Character.QueryWith[EmptyType, TFieldData] = new Character.QueryWith(new EmptyQuery())(new Character.Field[EmptyType, TFieldData])

  implicit def characterHasAge[T](query: Age.QueryWith[EmptyType, T]): Character.CharacterFieldQuery[EmptyType, Age.TypedWith[EmptyType, Int]] = new Character.CharacterFieldQuery(query.inner)(new Age.Field[EmptyType, Int])

  object Age {
    class With[TFieldData] {
      def ::[TInner](inner: TInner) = new TypedWith[TInner, TFieldData](inner)
    }
    class TypedWith[TInner, TFieldData](val inner: TInner) extends FieldTrait[TFieldData]
    implicit def innerObject[TInner, TFieldData](outer: TypedWith[TInner, TFieldData]): TInner = outer.inner
    trait FieldTrait[TFieldData] {
      var age: TFieldData = _
    }

    class Field[TInner, TFieldData] extends AbstractField[TInner, TypedWith[TInner, TFieldData]] {
      def name = "age"
      def addTrait(innerParseResult: TInner) = innerParseResult :: new With[TFieldData]()
    }
    class QueryWith[TInner, TFieldData](val inner: AbstractQuery[TInner])(val field: AbstractField[TInner, TFieldData]) extends FieldQuery(inner)(field)
  }

  def age = new Age.QueryWith(new EmptyQuery())(new Age.Field[EmptyType, AnyType])

  object Query {
    class Query[TResponse](innerQuery: AbstractQuery[TResponse]) extends AbstractQuery[TResponse] {
      def parseResponse(response: String) = innerQuery.parseResponse(response)
      def generateQuery() = "query"
    }
    class QueryFieldQuery[TResponse, TWrapper](innerQuery: AbstractQuery[TResponse])(field: AbstractField[TResponse, TWrapper]) extends FieldQuery[TResponse, TWrapper](innerQuery)(field) {
      def character() = new QueryFieldQuery(this)(new Character.Field[TWrapper, AnyType])
    }
  }

  def query[TFieldData](subQuery: Query.QueryFieldQuery[EmptyType, TFieldData]) = new Query.Query(subQuery)

  implicit def queryHasCharacter[A](query: Character.QueryWith[EmptyType, A]) = new Query.QueryFieldQuery(query.inner)(new Character.Field[EmptyType, A])

  abstract trait AbstractQuery[A] {
    def generateQuery(): String
    def parseResponse(response: String): A
  }

  class EmptyType {}
  class EmptyQuery extends AbstractQuery[EmptyType] {
    def generateQuery() = ""
    def parseResponse(response: String) = new EmptyType()
  }

  class AnyType {}

  class FieldQuery[TInner, TWrapper](inner: AbstractQuery[TInner])(field: AbstractField[TInner, TWrapper]) extends AbstractQuery[TWrapper] {
    def generateQuery(): String = field.name // or something like that
    def parseResponse(response: String): TWrapper = field.addTrait(inner.parseResponse(response))
  }

  abstract class AbstractField[TInner, TWrapper] {
    def name: String
    def addTrait(innerParseResult: TInner): TWrapper
  }


  def askForCharacter() = {
    send(query(
      character()(
        age
      )
    ))
  }

  def useCharacter() = {
    val response = askForCharacter()
    response.map(_.character.age)
  }
}
