package scalagraphqlclient.schema.generating
import scalagraphqlclient.schema.parsing._
import scalagraphqlclient.schema.converting._

class DslGenerator {
  def generate(schemaModel: InternalSchemaModel): String = {
    val types: String = generate(schemaModel.typeDefinitions)
    val fields: String = getCompanionFieldObjects(schemaModel.fields).mkString("\n")
    s"""import scala.concurrent._
import ExecutionContext.Implicits.global
object Client {
  def send[A](query: Query[A]) = Future {
    query.parseResponse("")
  }

  ${types}
  ${fields}

  abstract trait AbstractQuery[A] {
    def generateQuery(): String
    def parseResponse(response: String): A
  }

  class Query[A](inner: AbstractQuery[A]) extends AbstractQuery[A] {
    def generateQuery() = inner.generateQuery()
    def parseResponse(response: String) = inner.parseResponse(response)
  }

  class EmptyType {}
  class EmptyQuery extends AbstractQuery[EmptyType] {
    def generateQuery() = ""
    def parseResponse(response: String) = new EmptyType()
  }

  class AnyType {}

  def query[A](inner: AbstractQuery[A]) = new Query(inner)

  class FieldQuery[TInner, TWithField](inner: AbstractQuery[TInner])(field: AbstractField[TInner, TWithField]) extends AbstractQuery[TWithField] {
    def generateQuery(): String = field.name // or something like that
    def parseResponse(response: String): TWithField = field.addTrait(inner.parseResponse(response))
  }

  abstract class AbstractField[TInner, TWithField] {
    def name: String
    def addTrait(innerParseResult: TInner): TWithField
  }
}
"""
  }

  def generate(typeDefinition: InternalTypeDefinition) = {
    val name = typeDefinition.definedType.name.capitalize
    val fieldMethods = generateFieldMethods(typeDefinition.fields, name)
    val lowerName = typeDefinition.definedType.name
    s"""
object ${name} {
    class With[TFieldData] {
      implicit def innerObject[TInner](outer: TypedWith[TFieldData, TInner]): TInner  = outer.inner
      def ::[TInner](typed: TInner) = new TypedWith[TFieldData, TInner](typed)
    }
    class Field[TInner, TFieldData] extends AbstractField[TInner, TypedWith[TFieldData, TInner]] {
      def name = "${lowerName}"
      def addTrait(innerParseResult: TInner) = innerParseResult :: new With[TFieldData]()
    }
    trait FieldTrait[TFieldData] {
      var character: TFieldData = _
    }
    class TypedWith[TFieldData, TInner](val inner: TInner) extends FieldTrait[TFieldData]

    class QueryWith[TInner, TFieldData](val inner: AbstractQuery[TInner])(val field: Field[TInner, TFieldData]) extends FieldQuery(inner)(field)

    trait ${name}FieldQuery[TInner, TWithField] extends AbstractQuery[TWithField] {
      ${fieldMethods}
    }
}
"""
  }

  def generateFieldMethods(fields: Seq[InternalFieldReference], parentTypeName: String) = fields.map(field => {
    val methodName = field.fieldName
    val fieldType = field.fieldType.code
    val fieldNameCapitalized = field.fieldName.capitalize
    s"def ${methodName}() = new ${fieldNameCapitalized}.QueryWith(this)(new ${fieldNameCapitalized}.Field[TWithField, ${fieldType}]())"
  }).mkString("\n")

  def generate(types: Seq[InternalTypeDefinition]): String = types.map(generate).mkString("\n")

  def getCompanionFieldObjects(fields: Seq[InternalFieldName]) = fields.map(generateCompanionObject(_))

  def generateCompanionObject(field: InternalFieldName) = {
    val name = field.fieldName
    val objectName = field.fieldName.capitalize
    val typeDescriptions = field.fieldTypes
    val implicitConversions = for (fieldType <- field.fieldTypes)
    yield s"implicit def ${name}As${fieldType.description}(queryWith: ${objectName}.QueryWith[EmptyType, AnyType]) = new ${objectName}.QueryWith(queryWith.inner)(queryWith)(new ${objectName}.Field[EmptyType, ${fieldType.code}])"
    s"""
object ${objectName} {
    class With[TFieldData] {
      implicit def innerObject[TInner](outer: TypedWith[TFieldData, TInner]): TInner  = outer.inner
      def ::[TInner](typed: TInner) = new TypedWith[TFieldData, TInner](typed)
    }
    class TypedWith[TFieldData, TInner](val inner: TInner) extends FieldTrait[TFieldData]
    trait FieldTrait[TFieldData] {
      var ${name}: TFieldData = _
    }

    class Field[TInner, TFieldData] extends AbstractField[TInner, TypedWith[TFieldData, TInner]] {
      def name = "${name}"
      def addTrait(innerParseResult: TInner) = innerParseResult :: new With[TFieldData]()
    }
    class QueryWith[TInner, TFieldData](val inner: AbstractQuery[TInner])(val field: Field[TInner, TFieldData]) extends FieldQuery(inner)(field)
}

def ${name} = new ${objectName} .QueryWith(new EmptyQuery())(new ${objectName}.Field[EmptyType, AnyType])
"""
  }
}
