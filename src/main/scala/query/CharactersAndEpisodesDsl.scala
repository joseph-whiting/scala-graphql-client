package com.example.graphqlclient

class Query[A] {
  def fields(
    field1: Query[A] => Query[A] with WithField,
  ) = field1(this)
}

class Character {}

class CharacterQuery extends Query[Character] {
  def withName(): CharacterQuery with WithName = CharacterWithName
}

object CharacterWithName extends CharacterQuery with WithName {
  def name = "Luke"
}

trait WithField
trait WithName extends WithField {
  def name: String
}

trait WithAge extends WithField {
  def age: Int
}

object MyQuery extends CharacterQuery {}

object User {
  val name = MyQuery.withName().name
}
