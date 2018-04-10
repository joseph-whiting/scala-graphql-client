package scalagraphqlclient.schema.generating
import scalagraphqlclient.schema.parsing._
import org.scalatest._

class SchemaParserSpec extends FunSpec {
  describe("the dsl code generator") {
    val generator = new Generator()
    describe("for a basic Star wars example with characters with names") {
      val outputCode: String = generator.generate(Seq(
        TypeDefinition(DefinedType("character"), Seq(Field("name", GraphQLString)))
                                                  ))
      val basicQuery: String = """
  Client.send(query (
character (
name
)
))"""
      it("should produce code which compiles") {
        assertCompiles("object Test {  }")
      }

      it("should produce a client class") {
        assertCompiles(outputCode + "\nClient")
      }

      it("should allow the basic query to be sent") {
        assertCompiles(outputCode + basicQuery)
      }

      it("should allow the character field to be accessed on the response") {
        assertCompiles(outputCode + basicQuery + ".map(_.character)")
      }

      it("should allow the character's name field to be accessed on the response") {
        assertCompiles(outputCode + basicQuery + ".map(_.character(2).name)")
      }
    }
  }
}
