package scalagraphqlclient.schema.converting

import scalagraphqlclient.schema.parsing._

case class InternalField(fieldName: String, fieldType: InternalType, fieldKey: String)

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

  case class FieldsAndMap(fields: Seq[InternalField], map: Map[String, Int])

  def convert(typeDefinition: TypeDefinition): InternalTypeDefinition = {
    val fieldsWithMap = makeUniqueFields(typeDefinition.fields)
    InternalTypeDefinition(convert(typeDefinition.definedType), fieldsWithMap.fields)
  }

  def convert(definedType: DefinedType): InternalDefinedType = InternalDefinedType(definedType.name)

  def convert(field: Field, key: String): InternalField = InternalField(field.fieldName, convert(field.fieldType), key)

  def convert(graphQLType: GraphQLType): InternalType = graphQLType match {
    case Required(GraphQLString) => InternalString
    case Required(GraphQLInt) => InternalInt
    case Required(GraphQLBoolean) => InternalBoolean
    case Required(GraphQLList(inner: GraphQLType)) => InternalSeq(convert(inner))
    case Required(DefinedType(name: String)) => InternalDefinedType(name)
    case notRequired: GraphQLType => InternalOption(convert(Required(notRequired)))
  }

  def makeUniqueFields(fields: Seq[Field]) = {
    fields.foldLeft(FieldsAndMap(Seq(), Map[String, Int]())){(fieldsAndMap: FieldsAndMap, graphQLField: Field) => fieldsAndMap match {
        case FieldsAndMap(fields, currentMap) => {
          val name: String = graphQLField.fieldName
          val count: Int = currentMap.getOrElse(name, 0)
          val nextCount: Int = count + 1
          val fieldKey: String = s"name${nextCount}"
          val field: InternalField = convert(graphQLField, fieldKey)
          FieldsAndMap(fields :+ field, currentMap + (graphQLField.fieldName -> nextCount))
        }
    }
    }
  }
}

