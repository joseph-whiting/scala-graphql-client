package com.example.graphqlclient

object Client {
  type Request[T] = String => TypedWithField[T]

  type RequestDeclaration[T] = GraphQLResponse => TypedWithField[T]

  class GraphQLResponse {}

  def character[T](declaration: RequestDeclaration[T]): Request[T] = {
    responseJSON =>
      {
        declaration(new GraphQLResponse())
      }

  }
  implicit val characterFactory = new CharacterFactory()

  def fields[T](field1: Field[T])(
      implicit tFactory: Factory[T]): RequestDeclaration[T] = { response =>
    field1(response, tFactory.makeNew())
  }

  abstract class Factory[T] {
    def makeNew(): T
  }

  class Character {}
  class CharacterFactory extends Factory[Character] {
    def makeNew() = new Character
  }

  type Field[T] = (GraphQLResponse, T) => TypedWithField[T]

  def name[T](eav: GraphQLResponse, typed: T): WithName.TypedWithName[T] =
    typed :: WithName

  abstract trait WithField

  trait WithName {
    def name: String = "Luke Skywalker"
  }

  abstract class TypedWithField[T]

  object WithName {
    implicit def innerObject[T](outer: TypedWithName[T]): T = outer.inner
    def ::[T](typed: T) = new TypedWithName[T](typed)
    class TypedWithName[T] private[WithName] (val inner: T)
        extends TypedWithField[T]
        with WithName
  }

  def getCharacterWithName = character {
    fields(name[Character])
  }

}
