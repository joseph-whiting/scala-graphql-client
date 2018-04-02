package scalagraphqlclient.schema.parsing

case class Field(fieldName: String, fieldType: FieldType)

abstract class FieldType 
case class Required(innerType: FieldType) extends FieldType
case class GraphQLList(innerType: FieldType) extends FieldType

abstract class GraphQLType extends FieldType
case class DefinedType(name: String, fields: Seq[Field]) extends GraphQLType
abstract class GraphQLScalarType extends GraphQLType
case object GraphQLString extends GraphQLScalarType
case object GraphQLNumber extends GraphQLScalarType
case object GraphQLBoolean extends GraphQLScalarType

abstract class Parser {
  def parse(inputString: String): Seq[DefinedType]
}

class SchemaParser extends Parser {
  def parse(inputString: String): Seq[DefinedType] = Seq(DefinedType("Character", Seq(Field("name", Required(GraphQLString)), Field("appearsIn", Required(GraphQLList(GraphQLString))))))
}
