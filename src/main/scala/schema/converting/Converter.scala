package scalagraphqlclient.schema.converting

import scalagraphqlclient.schema.parsing._

case class InternalFieldName(fieldName: String, fieldTypes: Set[InternalType])
case class InternalFieldReference(fieldName: String, fieldType: InternalType) // could get a collision if there were a type with "of" in its name ?

abstract class InternalType {
  def description: String
  def code: String
}
class InternalWrapperType(name: String, inner: InternalType) extends InternalType {
  def description: String = s"${name}Of${inner.description}"
  def code: String = s"${name}[${inner.code}]"
}
case class InternalOption(inner: InternalType) extends InternalWrapperType("Option", inner)
case class InternalSeq(inner: InternalType) extends InternalWrapperType("Seq", inner)
class InternalNonWrapperType(name: String) extends InternalType {
  def description: String = name
  def code: String = name
}
class InternalScalarType(name: String) extends InternalNonWrapperType(name)
case object InternalString extends InternalScalarType("String")
case object InternalInt extends InternalScalarType("Int")
case object InternalBoolean extends InternalScalarType("Boolean")
case class InternalDefinedType(name: String) extends InternalNonWrapperType(name)

case class InternalTypeDefinition(definedType: InternalDefinedType, fields: Seq[InternalFieldReference])

case class InternalSchemaModel(typeDefinitions: Seq[InternalTypeDefinition], fields: Seq[InternalFieldName])

abstract trait GraphQLtoInternalConverter {
  def convert(typeDefinitions: TypeDefinition): InternalSchemaModel
}

object GraphQLtoInternalConverter {
  def convert(typeDefinitions: Seq[TypeDefinition]): InternalSchemaModel = {
    val internalTypeDefintions = getInternalTypes(typeDefinitions)
    val fields = getFieldsByName(internalTypeDefintions.flatMap(_.fields))
    InternalSchemaModel(internalTypeDefintions, fields)
  }

  def getInternalTypes(typeDefinitions: Seq[TypeDefinition]): Seq[InternalTypeDefinition] = typeDefinitions.map(convert(_))


  def convert(typeDefinition: TypeDefinition): InternalTypeDefinition = {
    InternalTypeDefinition(convert(typeDefinition.definedType), typeDefinition.fields.map(convert(_)))
  }

  def convert(definedType: DefinedType): InternalDefinedType = InternalDefinedType(definedType.name)

  def convert(field: Field): InternalFieldReference = {
    val converted = convert(field.fieldType)
    InternalFieldReference(field.fieldName, converted)
  }

  def getFieldsByName(fields: Seq[InternalFieldReference]): Seq[InternalFieldName] = {
    val fieldsByName = fields.groupBy(_.fieldName)
    fieldsByName.toSeq.map(tuple => {
      val (name, innerFields) = tuple
      val fieldTypeDescriptions = innerFields.map(_.fieldType).toSet
      InternalFieldName(name, fieldTypeDescriptions)
    })
  }

  def convert(graphQLType: GraphQLType): InternalType = graphQLType match {
    case Required(GraphQLString) => InternalString
    case Required(GraphQLInt) => InternalInt
    case Required(GraphQLBoolean) => InternalBoolean
    case Required(GraphQLList(inner: GraphQLType)) => InternalSeq(convert(inner))
    case Required(DefinedType(name: String)) => InternalDefinedType(name)
    case notRequired: GraphQLType => InternalOption(convert(Required(notRequired)))
  }
}
