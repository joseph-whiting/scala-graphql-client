package scalagraphqlclient.schema.converting

import scalagraphqlclient.schema.parsing._

case class InternalField(fieldName: String, fieldType: InternalType)

abstract class InternalType
case class InternalOption(inner: InternalType) extends InternalType
case class InternalSeq(inner: InternalType) extends InternalType
abstract class InternalScalarType extends InternalType
case object InternalString extends InternalScalarType
case object InternalInt extends InternalScalarType
case object InternalBoolean extends InternalScalarType
case class InternalDefinedType(name: String) extends InternalType

case class InternalTypeDefinition(definedType: InternalDefinedType, fields: Seq[InternalField])

abstract trait GraphQLtoInternalConverter {
  def convert(typeDefinitions: TypeDefinition): Seq[InternalTypeDefinition]
}

object GraphQLtoInternalConverter {
  def convert(typeDefinitions: Seq[TypeDefinition]): Seq[InternalTypeDefinition] = typeDefinitions.map(convert(_))
  def convert(typeDefinition: TypeDefinition): InternalTypeDefinition = InternalTypeDefinition(convert(typeDefinition.definedType), typeDefinition.fields.map(convert(_)))
  def convert(definedType: DefinedType): InternalDefinedType = InternalDefinedType(definedType.name)
  def convert(field: Field): InternalField = InternalField(field.fieldName, convert((field.fieldType)))
  def convert(graphQLType: GraphQLType): InternalType = graphQLType match {
    case Required(GraphQLString) => InternalString
    case Required(GraphQLInt) => InternalInt
    case Required(GraphQLBoolean) => InternalBoolean
    case Required(GraphQLList(inner: GraphQLType)) => InternalSeq(convert(inner))
    case Required(DefinedType(name: String)) => InternalDefinedType(name)
    case notRequired: GraphQLType => InternalOption(convert(Required(notRequired)))
  }
}
