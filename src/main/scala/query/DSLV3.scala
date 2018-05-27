import scala.concurrent._
import ExecutionContext.Implicits.global

object Client {
  def send[A, B](query: Query.QueryWith[A, B]) = Future {
    query.parseResponse("")
  }

  object Query {
    class With[TFieldData] {
      def ::[TInner](typed: TInner) = new TypedWith[TInner, TFieldData](typed)
    }
    class Field[TParent, TField] extends AbstractField[TParent, TypedWith[TParent, TField]] {
      def name = "query"
      def addTrait(innerParseResult: TParent) = innerParseResult :: new With[TField]()
    }
    trait FieldTrait[TFieldData] {
      var query: TFieldData = _
    }
    class TypedWith[TInner, TFieldData](val inner: TInner) extends FieldTrait[TFieldData]
    implicit def innerObject[TInner, TFieldData](outer: TypedWith[TInner, TFieldData]): TInner = outer.inner
    class QueryWith[TResponse, TFieldData](val inner: AbstractQuery[TResponse])(val field: Field[TResponse, TFieldData]) extends FieldQuery(inner)(field)

    class QueryFieldQuery[TResponse, TWrapper](innerQuery: AbstractQuery[TResponse])(field: AbstractField[TResponse, TWrapper]) extends FieldQuery[TResponse, TWrapper](innerQuery)(field) {
      def character[TFieldData](subQuery: Character.CharacterFieldQuery[EmptyType, TFieldData]) = new QueryFieldQuery(this)(new Character.Field[TWrapper, TFieldData])
    }
  }

  def query[TFieldData](subQuery: Query.QueryFieldQuery[EmptyType, TFieldData]): Query.QueryWith[EmptyType, TFieldData] = new Query.QueryWith(new EmptyQuery())(new Query.Field[EmptyType, TFieldData])

  implicit def queryHasCharacter[A](query: Character.QueryWith[EmptyType, A]): Query.QueryFieldQuery[EmptyType, Character.TypedWith[EmptyType, A]] = new Query.QueryFieldQuery(query.inner)(new Character.Field[EmptyType, A])


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
def name() = new CharacterFieldQuery(this)(new Name.Field[TWrapper, String])
    }
  }

  def character[TFieldData](subQuery: Character.CharacterFieldQuery[EmptyType, TFieldData]): Character.QueryWith[EmptyType, TFieldData] = new Character.QueryWith(new EmptyQuery())(new Character.Field[EmptyType, TFieldData])

  implicit def characterHasAge[A](query: Age.QueryWith[EmptyType, A]): Character.CharacterFieldQuery[EmptyType, Age.TypedWith[EmptyType, Int]] = new Character.CharacterFieldQuery(query.inner)(new Age.Field[EmptyType, Int])
implicit def characterHasName[A](query: Name.QueryWith[EmptyType, A]): Character.CharacterFieldQuery[EmptyType, Name.TypedWith[EmptyType, String]] = new Character.CharacterFieldQuery(query.inner)(new Name.Field[EmptyType, String])


  object Age {
    class With[TFieldData] {
      def ::[TInner](typed: TInner) = new TypedWith[TInner, TFieldData](typed)
    }
    class Field[TParent, TField] extends AbstractField[TParent, TypedWith[TParent, TField]] {
      def name = "age"
      def addTrait(innerParseResult: TParent) = innerParseResult :: new With[TField]()
    }
    trait FieldTrait[TFieldData] {
      var age: TFieldData = _
    }
    class TypedWith[TInner, TFieldData](val inner: TInner) extends FieldTrait[TFieldData]
    implicit def innerObject[TInner, TFieldData](outer: TypedWith[TInner, TFieldData]): TInner = outer.inner
    class QueryWith[TResponse, TFieldData](val inner: AbstractQuery[TResponse])(val field: Field[TResponse, TFieldData]) extends FieldQuery(inner)(field)

    class AgeFieldQuery[TResponse, TWrapper](innerQuery: AbstractQuery[TResponse])(field: AbstractField[TResponse, TWrapper]) extends FieldQuery[TResponse, TWrapper](innerQuery)(field) {

    }
  }

  def age: Age.QueryWith[EmptyType, AnyType] = new Age.QueryWith(new EmptyQuery())(new Age.Field[EmptyType, AnyType])




  object Name {
    class With[TFieldData] {
      def ::[TInner](typed: TInner) = new TypedWith[TInner, TFieldData](typed)
    }
    class Field[TParent, TField] extends AbstractField[TParent, TypedWith[TParent, TField]] {
      def name = "name"
      def addTrait(innerParseResult: TParent) = innerParseResult :: new With[TField]()
    }
    trait FieldTrait[TFieldData] {
      var name: TFieldData = _
    }
    class TypedWith[TInner, TFieldData](val inner: TInner) extends FieldTrait[TFieldData]
    implicit def innerObject[TInner, TFieldData](outer: TypedWith[TInner, TFieldData]): TInner = outer.inner
    class QueryWith[TResponse, TFieldData](val inner: AbstractQuery[TResponse])(val field: Field[TResponse, TFieldData]) extends FieldQuery(inner)(field)

    class NameFieldQuery[TResponse, TWrapper](innerQuery: AbstractQuery[TResponse])(field: AbstractField[TResponse, TWrapper]) extends FieldQuery[TResponse, TWrapper](innerQuery)(field) {

    }
  }

  def name: Name.QueryWith[EmptyType, AnyType] = new Name.QueryWith(new EmptyQuery())(new Name.Field[EmptyType, AnyType])




  implicit def skipQuery[A](outer: Query.TypedWith[EmptyType, A]): A = outer.query

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

  def getNames() = send(query(character( name ))).map(_.character.name)
}
