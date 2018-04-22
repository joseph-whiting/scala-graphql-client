package scalagraphqlclient.schema.converting

import scalagraphqlclient.schema.parsing._

abstract class InternalType
case class InternalString() extends InternalType
case class InternalInt() extends InternalType
case class InternalOption(inner: InternalType) extends InternalType
case class InternalSeq(inner: InternalType) extends InternalType
case class InternalDefinedType(name: String) extends InternalType

object Converter {
  def convert(graphQLType: FieldType): InternalType = graphQLType match {
    case Required(GraphQLString) => InternalString()
    case Required(GraphQLInt) => InternalInt()
    case Required(definedType: DefinedType) => InternalDefinedType(definedType.name)
    case notRequired: GraphQLType => InternalOption(convert(Required(notRequired)))
  }
}
