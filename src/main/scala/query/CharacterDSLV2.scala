package com.example.graphqlclientdsl
import scala.concurrent._
import ExecutionContext.Implicits.global

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

object Client {
  def send[A](query: Query[A]) = Future {
    query.parseResponse("")
  }

  abstract trait AbstractQuery[A] {
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

  class CharacterQuery[A, B](fields: AbstractCharacterFieldQuery[A])(inner: AbstractQuery[B]) extends AbstractQuery[TypedWithCharacters[A, B]] {
    def generateQuery() = ""
    def parseResponse(response: String) = inner.parseResponse(response) :: new WithCharacters[A]()
  }

  abstract class AbstractCharacterFieldQuery[A] extends AbstractQuery[A]

  class CharacterFieldQuery[A, B](inner: AbstractQuery[A])(field: CharacterField[A, B]) extends AbstractCharacterFieldQuery[B] {
    def age() = new CharacterFieldQuery(this)(new AgeField[B]())
    def name() = new CharacterFieldQuery(this)(new NameField[B]())
    def generateQuery() = field.name // or something like that
    def parseResponse(response: String) = field.addTrait(inner.parseResponse(response))
  }

  abstract class CharacterField[A, B] {
    def name: String
    def addTrait(innerParseResult: A): B
  }

  class NameField[A] extends CharacterField[A, WithName.TypedWithName[A]] {
    def name = "name"
    def addTrait(innerParseResult: A) = innerParseResult :: WithName
  }

  class AgeField[A] extends CharacterField[A, WithAge.TypedWithAge[A]] {
    def name = "age"
    def addTrait(innerParseResult: A) = innerParseResult :: WithAge
  }

  class EmptyQuery extends AbstractQuery[EmptyType] {
    def generateQuery() = ""
    def parseResponse(response: String) = new EmptyType()
  }

  def query[A](inner: AbstractQuery[A]) = new Query(inner)
  def character[A]()(fields: AbstractCharacterFieldQuery[A]) = new CharacterQuery(fields)(new EmptyQuery())
  def name = new CharacterFieldQuery(new EmptyQuery())(new NameField[EmptyType]())
  def age = new CharacterFieldQuery(new EmptyQuery())(new AgeField[EmptyType]())
  }
