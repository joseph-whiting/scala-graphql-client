import scala.tools.reflect.ToolBox
import scala.tools.reflect.ToolBoxError
import scala.reflect.runtime.universe
import scalagraphqlclient.schema.generating._
import scalagraphqlclient.schema.parsing._
import scala.io.Source
import org.scalatest._

class DslGeneratorSpec extends FunSpec {
  describe("the DSL generator") {
    val tb = universe.runtimeMirror(getClass.getClassLoader).mkToolBox()
    val generator = new DslGenerator()
    describe("for a basic character type with name and age") {
      val schemaModel: Seq[TypeDefinition] = Seq(
        TypeDefinition(
          DefinedType("Character"),
          Seq(
            Field("name", Required(GraphQLString)),
            Field("age", Required(GraphQLInt))
          )
        )
      )
      val generatedCode = generator.generate(schemaModel)
      it("should generate code that compiles") {
        tb.compile(tb.parse(generatedCode))
      }

      def assertAllowsUsage(usage: String) = {
        tb.compile(tb.parse(s"""
${generatedCode}
import Client._
object Test {
   ${usage}
}
"""))
      }

      def assertDoesNotAllowUsage(usage: String) = {
        assertThrows[ToolBoxError] {
          assertAllowsUsage(usage)
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
  }
}
