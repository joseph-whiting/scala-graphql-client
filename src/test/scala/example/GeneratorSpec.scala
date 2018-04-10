import scala.tools.reflect.ToolBox
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
                Field("name", GraphQLString),
                Field("age", GraphQLInt)
              )
            )
          )
          val generatedCode = generator.generate(schemaModel)
          it("should generate code that compiles"){
            tb.compile(tb.parse(generatedCode))
          }
        }
      }
}
