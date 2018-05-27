package scalagraphqlclient.schema.converting

import scalagraphqlclient.schema.parsing._

case class InternalSchemaModel(internalObjects: Seq[InternalObject])
case class InternalObject(objectName: String, methodName: String, fields: Seq[InternalField])
case class InternalField(fieldName: String, fieldObjectName: String, fieldType: InternalFieldType)
abstract class InternalFieldType
case class InternalObjectType() extends InternalFieldType
case class InternalScalarType(val scalaCode: String) extends InternalFieldType
case class InternalWrappingType[A <: InternalFieldType](val inner: A, val scalaCode: String) extends InternalFieldType

abstract trait GraphQLtoInternalConverter {
  def convert(typeDefinitions: TypeDefinition): InternalSchemaModel
}

object GraphQLtoInternalConverter {
  def convert(definitions: Seq[TypeDefinition]): InternalSchemaModel = InternalSchemaModel(Seq())
}
