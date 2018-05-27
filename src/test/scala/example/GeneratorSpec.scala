import scala.tools.reflect.ToolBox
import scala.tools.reflect.ToolBoxError
import scala.reflect.runtime.universe
import scalagraphqlclient.schema.generating._
import scalagraphqlclient.schema.converting._
import scala.io.Source
import org.scalatest._
import java.io._

class DslGeneratorSpec extends FunSpec {
  describe("the DSL generator") {
    val tb = universe.runtimeMirror(getClass.getClassLoader).mkToolBox()
    val generator = new DslGenerator()
    describe("for a basic character type with required name and age") {
      val schemaModel = InternalSchemaModel(
        Seq(
          InternalObject("Query", "query", Seq(InternalField("character", "Character", InternalObjectType()))),
          InternalObject("Character", "character", Seq(
            InternalField("age", "Age", InternalScalarType("Int")),
            InternalField("name", "Name", InternalScalarType("String"))
          )),
          InternalObject("Age", "age", Seq()),
          InternalObject("Name", "name", Seq())
        )
      )
      val generatedCode = generator.generate(schemaModel)
      def assertAllowsUsage(usage: String) = assertCodeAllowsUsage(usage, generatedCode)
      def assertDoesNotAllowUsage(usage: String) = assertCodeDoesNotAllowUsage(usage, generatedCode)
      it("should generate code that compiles") {
        try {
          tb.compile(tb.parse(generatedCode))
        } catch {
          case _ => {
            println(s"""Generated DSL code failed to compile:

${generatedCode}
""")
            throw new Exception("Generated DSL code failed to compile")
          }
        }
      }

      describe("given that the dsl compiles") {
        it("should generate a Client object") {
          assertAllowsUsage("Client")
        }
        it("should be able to send a basic query for character names") {
          assertAllowsUsage("""
Client.send(query(
  character(
    name
  )
))""")
        }
        it("should be able to get the names out of a character name query") {
          assertAllowsUsage("""
Client.send(query(
  character(
     name
  )
)).map(_.character.name)
""")
        }
        it("should NOT be  able to map the ages out of a character name query") {
          assertDoesNotAllowUsage("""
Client.send(query(
  character(
     name
  )
)).map(_.character.age)
""")
        }
        it("should allow a more complex query for characters with name and age") {
          assertAllowsUsage("""
Client.send(query(
  character(
    name
    age
  )
))
""")
        }

        it("should allow name and age to be accessed on name and age query") {
          assertAllowsUsage("""
val response = Client.send(query(
  character(
    age
    name
  )
))
response.map(_.character.name)
response.map(_.character.age)
""")
        }
      }
    }

    describe("for a character type with required name, non-required age and a requied list of their favourite drinks") {
      val schemaModel = InternalSchemaModel(
        Seq(
          InternalObject("Query", "query", Seq(InternalField("character", "Character", InternalObjectType()))),
          InternalObject("Character", "character", Seq(
              InternalField("age", "Age", InternalWrappingType(InternalScalarType("Int"), "Option")),
              InternalField("name", "Name", InternalScalarType("String")),
              InternalField("favouriteDrinks", "FavouriteDrinks", InternalWrappingType(InternalScalarType("String"), "Seq"))
          )),
          InternalObject("Age", "age", Seq()),
          InternalObject("Name", "name", Seq()),
          InternalObject("FavouriteDrinks", "favouriteDrinks", Seq())
        )
      )
      val generatedCode = generator.generate(schemaModel)
      def assertAllowsUsage(usage: String) = assertCodeAllowsUsage(usage, generatedCode)
      def assertDoesNotAllowUsage(usage: String) = assertCodeDoesNotAllowUsage(usage, generatedCode)
      it("should generate code that compiles") {
        try {
          tb.compile(tb.parse(generatedCode))
        } catch {
          case _ => {
            println(s"""Generated DSL code failed to compile:

${generatedCode}
""")
            throw new Exception("Generated DSL code failed to compile")
          }
        }
      }

      it("should allow access to the age through an option") {
        assertAllowsUsage("""
val response = Client.send(query(
  character(
    age
  )
))
val ages: Future[Option[Int]] = response.map(_.character.age)
""")
      }

      it("should allow access to the favourite drinks through a sequence") {
        assertAllowsUsage("""
val response = Client.send(query(
  character(
    favouriteDrinks
  )
))
val favouriteDrinksResult: Future[Seq[String]] = response.map(_.character.favouriteDrinks)
""")
      }
    }

    def assertCodeAllowsUsage(usage: String, generatedCode: String) = {
      tb.compile(tb.parse(s"""
${generatedCode}
import Client._
object Test {
   ${usage}
}
"""))
    }

    def assertCodeDoesNotAllowUsage(usage: String, generatedCode: String) = {
      assertThrows[ToolBoxError] {
        assertCodeAllowsUsage(usage, generatedCode)
      }
    }
  }
}
