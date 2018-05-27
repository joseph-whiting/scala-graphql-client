package scalagraphqlclient.schema.generating
import scalagraphqlclient.schema.parsing._
import scalagraphqlclient.schema.converting._

class DslGenerator {
  def generateObject(internalObject: InternalObject): String = {
    val method: String = internalObject.fields.length match {
      case 0 => s"def ${internalObject.methodName}: ${internalObject.objectName}.QueryWith[EmptyType, AnyType] = new ${internalObject.objectName}.QueryWith(new EmptyQuery())(new ${internalObject.objectName}.Field[EmptyType, AnyType])"
      case _ => s"def ${internalObject.methodName}[TInner, TFieldData](subQuery: ${internalObject.objectName}.${internalObject.objectName}FieldQuery[TInner, TFieldData]): ${internalObject.objectName}.QueryWith[EmptyType, TFieldData] = new ${internalObject.objectName}.QueryWith(new EmptyQuery())(new ${internalObject.objectName}.Field[EmptyType, TFieldData])"
    }
    val fieldMethods: String = internalObject.fields.map(field => isSomeSortOfObject(field.fieldType) match {
      case true => s"def ${field.fieldName}[TFieldData](subQuery: ${field.fieldObjectName}.${field.fieldObjectName}FieldQuery[EmptyType, TFieldData]) = new ${internalObject.objectName}FieldQuery(this)(new ${field.fieldObjectName}.Field[TWrapper, TFieldData])"
      case false => s"def ${field.fieldName}() = new ${internalObject.objectName}FieldQuery(this)(new ${field.fieldObjectName}.Field[TWrapper, ${codeFor(field.fieldType)}])"
    }).mkString("\n")

    val implicitFieldConversions: String = internalObject.fields.map(field => {
      s"implicit def ${internalObject.methodName}Has${field.fieldObjectName}[A](query: ${field.fieldObjectName}.QueryWith[EmptyType, A]): ${internalObject.objectName}.${internalObject.objectName}FieldQuery[EmptyType, ${field.fieldObjectName}.TypedWith[EmptyType, ${codeFor(field.fieldType)}]] = new ${internalObject.objectName}.${internalObject.objectName}FieldQuery(query.inner)(new ${field.fieldObjectName}.Field[EmptyType, ${codeFor(field.fieldType)}])"
    }).mkString("\n")
    s"""
  object ${internalObject.objectName} {
    class With[TFieldData] {
      def ::[TInner](typed: TInner) = new TypedWith[TInner, TFieldData](typed)
    }
    class Field[TParent, TField] extends AbstractField[TParent, TypedWith[TParent, TField]] {
      def name = "${internalObject.methodName}"
      def addTrait(innerParseResult: TParent) = innerParseResult :: new With[TField]()
    }
    trait FieldTrait[TFieldData] {
      var ${internalObject.methodName}: TFieldData = _
    }
    class TypedWith[TInner, TFieldData](val inner: TInner) extends FieldTrait[TFieldData]
    implicit def innerObject[TInner, TFieldData](outer: TypedWith[TInner, TFieldData]): TInner = outer.inner
    class QueryWith[TResponse, TFieldData](val inner: AbstractQuery[TResponse])(val field: Field[TResponse, TFieldData]) extends FieldQuery(inner)(field)

    class ${internalObject.objectName}FieldQuery[TResponse, TWrapper](innerQuery: AbstractQuery[TResponse])(field: AbstractField[TResponse, TWrapper]) extends FieldQuery[TResponse, TWrapper](innerQuery)(field) {
      ${fieldMethods}
    }
  }

  ${method}

  ${implicitFieldConversions}
"""
  }

  def codeFor(fieldType: InternalFieldType): String = fieldType match {
    case InternalScalarType(scalaCode: String) => scalaCode
    case InternalWrappingType(inner: InternalFieldType, scalaCode: String) => s"${scalaCode}[${codeFor(inner)}]"
    case InternalObjectType() => "A"
  }

  def isSomeSortOfObject(fieldType: InternalFieldType): Boolean = fieldType match {
    case InternalScalarType(_: String) => false
    case InternalWrappingType(inner: InternalFieldType, _: String) => isSomeSortOfObject(inner)
    case InternalObjectType() => true
  }

  def generate(schemaModel: InternalSchemaModel): String = {
    val objects = schemaModel.internalObjects.map(generateObject(_)).mkString("\n")
    s"""import scala.concurrent._
import ExecutionContext.Implicits.global
object Client {
  def send[A, B](query: Query.QueryWith[A, B]) = Future {
    query.parseResponse("")
  }

  ${objects}

  implicit def skipQuery[A](outer: Query.TypedWith[EmptyType, A]) = outer.query

  abstract trait AbstractQuery[A] {
    def generateQuery(): String
    def parseResponse(response: String): A
  }

  class EmptyType {}
  class EmptyQuery extends AbstractQuery[EmptyType] {
    def generateQuery() = ""
    def parseResponse(response: String) = new EmptyType()
  }

  class AnyType {}

  class FieldQuery[TInner, TWrapper](inner: AbstractQuery[TInner])(field: AbstractField[TInner, TWrapper]) extends AbstractQuery[TWrapper] {
    def generateQuery(): String = field.name // or something like that
    def parseResponse(response: String): TWrapper = field.addTrait(inner.parseResponse(response))
  }

  abstract class AbstractField[TInner, TWrapper] {
    def name: String
    def addTrait(innerParseResult: TInner): TWrapper
  }
}
"""
  }
}
