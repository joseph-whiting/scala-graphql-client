import scala.concurrent._
import ExecutionContext.Implicits.global
object Client {
  def send[A](query: Query[A]) = Future {
    query.parseResponse("")
  }


  object Character {
    class With[TFieldData] {
      implicit def innerObject[TInner](outer: TypedWith[TFieldData, TInner]): TInner  = outer.inner
      def ::[TInner](typed: TInner) = new TypedWith[TFieldData, TInner](typed)
    }
    class Field[TInner, TFieldData] extends AbstractField[TInner, TypedWith[TFieldData, TInner]] {
      def name = "character"
      def addTrait(innerParseResult: TInner) = innerParseResult :: new With[TFieldData]()
    }
    trait FieldTrait[TFieldData] {
      var character: TFieldData = _
    }
    class TypedWith[TFieldData, TInner](val inner: TInner) extends FieldTrait[TFieldData]

    class QueryWith[TInner, TFieldData](val inner: AbstractQuery[TInner])(val field: Field[TInner, TFieldData]) extends FieldQuery(inner)(field)

    trait CharacterFieldQuery[TInner, TWithField] extends AbstractQuery[TWithField] {
      def age() = new Age.QueryWith(this)(new Age.Field[TWithField, Int]())
    }
  }



object Age {
    class With[TFieldData] {
      implicit def innerObject[TInner](outer: TypedWith[TFieldData, TInner]): TInner  = outer.inner
      def ::[TInner](typed: TInner) = new TypedWith[TFieldData, TInner](typed)
    }
    class TypedWith[TFieldData, TInner](val inner: TInner) extends FieldTrait[TFieldData]
    trait FieldTrait[TFieldData] {
      var age: TFieldData = _
    }

    class Field[TInner, TFieldData] extends AbstractField[TInner, TypedWith[TFieldData, TInner]] {
      def name = "age"
      def addTrait(innerParseResult: TInner) = innerParseResult :: new With[TFieldData]()
    }
    class QueryWith[TInner, TFieldData](val inner: AbstractQuery[TInner])(val field: Field[TInner, TFieldData]) extends FieldQuery(inner)(field)
}

def age = new Age.QueryWith(new EmptyQuery())(new Age.Field[EmptyType, AnyType])


  abstract trait AbstractQuery[A] {
    def generateQuery(): String
    def parseResponse(response: String): A
  }

  class Query[A](inner: AbstractQuery[A]) extends AbstractQuery[A] {
    def generateQuery() = inner.generateQuery()
    def parseResponse(response: String) = inner.parseResponse(response)
  }

  class EmptyType {}
  class EmptyQuery extends AbstractQuery[EmptyType] {
    def generateQuery() = ""
    def parseResponse(response: String) = new EmptyType()
  }

  class AnyType {}

  def query[A](inner: AbstractQuery[A]) = new Query(inner)

  class FieldQuery[TInner, TWithField](inner: AbstractQuery[TInner])(field: AbstractField[TInner, TWithField]) extends AbstractQuery[TWithField] {
    def generateQuery(): String = field.name // or something like that
    def parseResponse(response: String): TWithField = field.addTrait(inner.parseResponse(response))
  }

  abstract class AbstractField[TInner, TWithField] {
    def name: String
    def addTrait(innerParseResult: TInner): TWithField
  }
}
