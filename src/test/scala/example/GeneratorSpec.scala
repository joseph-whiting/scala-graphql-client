import scala.tools.reflect.ToolBox
import scala.tools.reflect.ToolBoxError
import scala.reflect.runtime.universe
import scalagraphqlclient.schema.generating._
import scalagraphqlclient.schema.converting._
import scala.io.Source
import org.scalatest._

class DslGeneratorSpec extends FunSpec {
  describe("the DSL generator") {
    val tb = universe.runtimeMirror(getClass.getClassLoader).mkToolBox()
    val generator = new DslGenerator()
    describe("for a basic character type with required name and age") {
      val schemaModel: Seq[InternalTypeDefinition] = Seq(
        InternalTypeDefinition(
          InternalDefinedType("character"),
          Seq(
            InternalField("name", InternalString),
            InternalField("age", InternalInt)
          )
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
  character()(
    name
  )
))""")
        }
        it("should be able to map the names out of a character name query") {
          assertAllowsUsage("""
Client.send(query(
  character()(
     name
  )
)).map(_.character.map(_.name))
""")
        }
        it("should NOT be  able to map the ages out of a character name query") {
          assertDoesNotAllowUsage("""
Client.send(query(
  character()(
     name
  )
)).map(_.character.map(_.age))
""")
        }
        it("should allow a more complex query for characters with name and age") {
          assertAllowsUsage("""
Client.send(query(
  character()(
    name
    age
  )
))
""")
        }

        it("should allow name and age to be accessed on name and age query") {
          assertAllowsUsage("""
val response = Client.send(query(
  character()(
    age
    name
  )
))
response.map(_.character.map(_.name))
response.map(_.character.map(_.age))
""")
        }
      }
    }

    describe("for a character type with required name, non-required age and a requied list of their favourite drinks") {
      val schemaModel: Seq[InternalTypeDefinition] = Seq(
        InternalTypeDefinition(
          InternalDefinedType("Character"),
          Seq(
            InternalField("name", InternalString),
            InternalField("age", InternalOption(InternalInt)),
            InternalField("favourite_drinks", InternalSeq(InternalString))
          )
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
          }
        }
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
