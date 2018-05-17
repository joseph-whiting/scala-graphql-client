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

  class FieldQuery[A, B](inner: AbstractQuery[A])(field: AbstractField[A, B]) extends AbstractQuery[B] {
    def generateQuery(): String = field.name // or something like that
    def parseResponse(response: String): B = field.addTrait(inner.parseResponse(response))
  }

  abstract class AbstractField[A, B] {
    def name: String
    def addTrait(innerParseResult: A): B
  }
}
"""
  }

  def generate(typeDefinition: InternalTypeDefinition) = {
    val name = typeDefinition.definedType.name.capitalize
    val fieldMethods = generateFieldMethods(typeDefinition.fields, name)
    val lowerName = name.toLowerCase()
    s"""
object ${name} {
    class ${name}Query[A, B](fields: Abstract${name}FieldQuery[A])(inner: AbstractQuery[B]) extends AbstractQuery[TypedWith${name}[A, B]] {
      def generateQuery() = ""
      def parseResponse(response: String) = inner.parseResponse(response) :: new With${name}[A]()
    }
    class With${name}[A] {
      implicit def innerObject[B](outer: TypedWith${name}[A, B]): B = outer.inner
      def ::[B](typed: B) = new TypedWith${name}[A, B](typed)
    }
    class TypedWith${name}[A, B] (val inner: B) extends ${name}[A]
    trait ${name}[A] {
      var ${lowerName}: Seq[A] = Seq()
    }
    abstract class Abstract${name}FieldQuery[A] extends AbstractQuery[A]

    class ${name}FieldQuery[A, B](inner: AbstractQuery[A])(field: AbstractField[A, B]) extends FieldQuery(inner)(field) {
      ${fieldMethods}
    }
}
"""
  }

  def generateFieldMethods(fields: Seq[InternalFieldReference], parentTypeName: String) = fields.map(field => {
    val methodName = field.fieldName
    val fieldType = field.fieldType.code
    val fieldNameCapitalized = field.fieldName.capitalize
    s"def ${methodName}() = new ${parentTypeName}.QueryWith(this)(new ${fieldNameCapitalized}FieldQuery(this)(new ${fieldNameCapitalized}.Field[B, ${fieldType}]()))"
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
    class With[A] {
      implicit def innerObject[B](outer: TypedWith[A, B]): B  = outer.inner
      def ::[B](typed: B) = new TypedWith[A, B](typed)
    }
    class TypedWith[A, B](val inner: B) extends FieldTrait[A]
    trait FieldTrait[A] {
      var ${name}: A = _
    }

    class Field[A, B] extends AbstractField[A, TypedWith[B, A]] {
      def name = "${name}"
      def addTrait(innerParseResult: A) = innerParseResult :: new With[B]()
    }
    class QueryWith[A, B](val inner: AbstractQuery[A])(val field: Field[A, B]) extends FieldQuery[A, B](inner)(field)
}

def ${field.fieldName} = new ${objectName}.QueryWith(new EmptyQuery())(new ${objectName}.Field[EmptyType, AnyType])
"""
  }
}
