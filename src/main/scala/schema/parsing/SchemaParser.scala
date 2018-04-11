package scalagraphqlclient.schema.parsing

case class Field(fieldName: String, fieldType: FieldType)

abstract class FieldType
case class Required(innerType: FieldType) extends FieldType
case class GraphQLList(innerType: FieldType) extends FieldType

abstract class GraphQLType extends FieldType
case class DefinedType(name: String) extends GraphQLType
abstract class GraphQLScalarType extends GraphQLType
case object GraphQLString extends GraphQLScalarType
case object GraphQLInt extends GraphQLScalarType
case object GraphQLFloat extends GraphQLScalarType
case object GraphQLID extends GraphQLScalarType
case object GraphQLBoolean extends GraphQLScalarType

case class TypeDefinition(definedType: DefinedType, fields: Seq[Field])
abstract class Parser {
  def parse(inputString: String): Seq[TypeDefinition]
}

class SchemaParser extends Parser {
  val typePattern = """(type )(.*) \{[\s]*((?:(?!type)[\s\S])*)\}""".r
  val fieldPattern = """([\S]*):[\s]*([\S]*)""".r
  val requiredFieldTypePattern = """([\S]*)!""".r
  val listFieldTypePattern = """\[(.*)\]""".r
  def parse(inputString: String): Seq[TypeDefinition] =
    typePattern.findAllIn(inputString).map(parseType _).toSeq
  def parseType(typeString: String): TypeDefinition = typeString match {
    case typePattern(_, name, fields) =>
      TypeDefinition(DefinedType(name), parseFields(fields))
  }
  def parseFields(fieldsString: String): Seq[Field] =
    fieldPattern.findAllIn(fieldsString).map(parseField _).toSeq
  def parseField(fieldString: String): Field = fieldString match {
    case fieldPattern(name, typeString) =>
      Field(name, parseFieldType(typeString))
  }
  def parseFieldType(fieldTypeString: String): FieldType =
    fieldTypeString match {
      case requiredFieldTypePattern(innerTypeString) =>
        Required(parseFieldType(innerTypeString))
      case listFieldTypePattern(innerTypeString) =>
        GraphQLList(parseFieldType(innerTypeString))
      case "String"  => GraphQLString
      case "Float"   => GraphQLFloat
      case "Int"     => GraphQLInt
      case "Boolean" => GraphQLBoolean
      case "ID"      => GraphQLID
    }
}
