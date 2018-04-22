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
            InternalField("name", InternalString, "name0"),
            InternalField("age", InternalInt, "age0")
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
            InternalField("name", InternalString, "name0"),
            InternalField("age", InternalOption(InternalInt), "age0"),
            InternalField("favourite_drinks", InternalSeq(InternalString), "favourite_drinks0")
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
            throw new Exception("Generated DSL code failed to compile")
          }
        }
      }

      it("should allow access to the age through an option") {
        assertAllowsUsage("""
val response = Client.send(query(
  character()(
    age
  )
))
val ages: Future[Seq[Option[Int]]] = response.map(_.character.map(_.age))
""")
      }

      it("should allow access to the favourite drinks through a sequence") {
        assertAllowsUsage("""
val response = Client.send(query(
  character()(
    favourite_drinks
  )
))
val favouriteDrinks: Future[Seq[Seq[String]]] = response.map(_.character.map(_.favourite_drinks))
""")
      }
    }

      describe("for a character type and an episode type, where characters have names and ages and appear in episodes, which have names and release numbers") {
      val schemaModel: Seq[InternalTypeDefinition] = Seq(
        InternalTypeDefinition(
          InternalDefinedType("Character"),
          Seq(
            InternalField("name", InternalString, "name0"),
            InternalField("age", InternalOption(InternalInt), "age0"),
            InternalField("appearsIn", InternalSeq(InternalDefinedType("Episode")), "appearsIn0")
          )
        ),
        InternalTypeDefinition(
          InternalDefinedType("Episode"),
          Seq(
            InternalField("name", InternalString, "name1"),
            InternalField("releaseNumber", InternalInt, "releaseNumber0"),
            InternalField("characters", InternalSeq(InternalDefinedType("Character")), "characters0")
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
            throw new Exception("Generated DSL code failed to compile")
          }
        }
      }

        it("should allow access to the names of episodes a character appears") {
          assertAllowsUsage("""
val response = Client.send(query(
  character()(
    appearsIn(
      releaseNumber
    )
  )
))
val releaseNumbersAppearedIn: Future[Seq[Seq[Int]]] = response.map(_.character.map(_.appearsIn.map(_.releaseNumber)))
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
