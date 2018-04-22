package scalagraphqlclient.schema.generating
import scalagraphqlclient.schema.parsing._

abstract class InternalType
abstract class InternalScalarType extends InternalType
case object InternalString extends InternalScalarType
case object InternalInt extends InternalScalarType
case class InternalOption(inner: InternalType) extends InternalType
case class InternalSeq(inner: InternalType) extends InternalType
case class InternalDefinedType(name: String) extends InternalType

class DslGenerator {
  def convert(graphQLType: FieldType): InternalType = graphQLType match {
    case Required(GraphQLString) => InternalString
    case Required(GraphQLInt) => InternalInt
    case Required(GraphQLList(inner: FieldType)) => InternalSeq(convert(inner))
    case Required(DefinedType(name: String)) => InternalDefinedType(name)
    case notRequired: GraphQLType => InternalOption(convert(Required(notRequired)))
  }
  def needsGenericTrait(internalType: InternalType): Boolean = internalType match {
    case InternalOption(inner: InternalType) => needsGenericTrait(inner)
    case InternalSeq(inner: InternalType) => needsGenericTrait(inner)
    case _: InternalScalarType => false
    case _ => true
  }
  def convertToScalaType(internalType: InternalType): String = internalType match {
    case InternalInt => "Int"
    case InternalString => "String"
    case InternalSeq(inner: InternalType) => {
      val innerName = convertToScalaType(inner)
      s"Seq[${innerName}]"
    }
    case InternalOption(inner: InternalType) => {
      val innerName = convertToScalaType(inner)
      s"Option[${innerName}]"
    }
    case InternalDefinedType(_: String) => "A"
  }
  def generateTraitForField(field: Field, parent: TypeDefinition): String = {
    val name: String = field.fieldName.toLowerCase()
    val capitalName: String = field.fieldName.capitalize
    val internalType = convert(field.fieldType)
    val scalaType: String = convertToScalaType(internalType)
    if(needsGenericTrait(internalType: InternalType)) {
      s"""
class With${capitalName}[A] {
  implicit def innerObject[T](outer: TypedWith${capitalName}[A, T]): T = outer.inner
  def ::[T](typed: T) = new TypedWith${capitalName}[A, T](typed)
}

class TypedWith${capitalName}[A, T](val inner: T) extends ${capitalName}[A]

trait ${capitalName}[A] {
  var ${name}: ${scalaType} = _
}
"""
    } else {
      s"""
object With${capitalName} {
  implicit def innerObject[T](outer: TypedWith${capitalName}[T]): T = outer.inner
  def ::[T](typed: T) = new TypedWith${capitalName}[T](typed)
  class TypedWith${capitalName}[T](val inner: T) extends ${capitalName}
}

trait ${capitalName} {
  var ${name}: ${scalaType} = _
}

class ${capitalName}Field[A] extends ${parent.definedType.name.capitalize}Field[A, With${capitalName}.TypedWith${capitalName}[A]] {
    def name = "${name}"
    def addTrait(innerParseResult: A) = innerParseResult :: With${capitalName}
  }
"""
    }
  }
  def generate(typeDefinition: TypeDefinition) = {
    val name = typeDefinition.definedType.name.toLowerCase()
    val capitalName = name.capitalize
    val fieldTraits = typeDefinition.fields.map(generateTraitForField(_, typeDefinition)).mkString("\n")
    val fieldMethods = typeDefinition.fields.map(_.fieldName)
      .map(fieldName => s"  def ${fieldName}() = new ${capitalName}FieldQuery(this)(new ${fieldName.capitalize}Field[B]())")
      .mkString("\n")
    val globalFieldMethods = typeDefinition.fields.map(_.fieldName)
      .map(fieldName => s"  def ${fieldName} = new ${capitalName}FieldQuery(new EmptyQuery())(new ${fieldName.capitalize}Field[EmptyType]())")
      .mkString("\n")
    s"""
class ${capitalName}Query[A, B](fields: Abstract${capitalName}FieldQuery[A])(inner: AbstractQuery[B]) extends AbstractQuery[TypedWith${capitalName}[A, B]] {
    def generateQuery() = ""
    def parseResponse(response: String) = inner.parseResponse(response) :: new With${capitalName}[A]()
  }
class With${capitalName}[A] {
  implicit def innerObject[B](outer: TypedWith${capitalName}[A, B]): B = outer.inner
  def ::[B](typed: B) = new TypedWith${capitalName}[A, B](typed)
}
class TypedWith${capitalName}[A, B] (val inner: B) extends ${capitalName}[A]
trait ${capitalName}[A] {
  var ${name}: Seq[A] = Seq()
}
abstract class Abstract${capitalName}FieldQuery[A] extends AbstractQuery[A]

class ${capitalName}FieldQuery[A, B](inner: AbstractQuery[A])(field: ${capitalName}Field[A, B]) extends AbstractCharacterFieldQuery[B] {
${fieldMethods}
  def generateQuery() = field.name // or something like that
  def parseResponse(response: String) = field.addTrait(inner.parseResponse(response))
}

abstract class ${capitalName}Field[A, B] {
  def name: String
  def addTrait(innerParseResult: A): B
}
${fieldTraits}

def ${name}[A]()(fields: Abstract${capitalName}FieldQuery[A]) = new ${capitalName}Query(fields)(new EmptyQuery())
${globalFieldMethods}
"""
  }
  def generate(types: Seq[TypeDefinition]): String = """
import scala.concurrent._
import ExecutionContext.Implicits.global
object Client {
  def send[A](query: Query[A]) = Future {
    query.parseResponse("")
  }

  abstract trait AbstractQuery[A] {
    def generateQuery(): String
    def parseResponse(response: String): A
  }

  class Query[A](inner: AbstractQuery[A]) extends AbstractQuery[A] {
    def generateQuery() = inner.generateQuery()
    def parseResponse(response: String) = inner.parseResponse(response)
  }

  class EmptyQuery extends AbstractQuery[EmptyType] {
    def generateQuery() = ""
    def parseResponse(response: String) = new EmptyType()
  }

  def query[A](inner: AbstractQuery[A]) = new Query(inner)

  class EmptyType {}
""" + types.map(generate).mkString("\n") + """
}
"""
}
