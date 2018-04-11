package scalagraphqlclient.schema.parsing

import scala.io.Source
import org.scalatest._

class SchemaParserSpec extends FunSpec {
  describe("The schema parser") {
    val parser = new SchemaParser()
    describe("for a simple Star Wars character example with only one type") {
      val outputDefinitions = parser.parse(
        """type Character {""" +
          """  name: String!""" +
          """  appearsIn: [String]!""" +
          """}"""
      )

      it("should get the correct name for the top level type") {
        assertResult("Character") {
          outputDefinitions(0).definedType.name
        }
      }

      it("should get the right number of fields") {
        assertResult(2) {
          outputDefinitions(0).fields.length
        }
      }

      it("should get the names of the fields correctly") {
        assertResult("name") {
          outputDefinitions(0).fields(0).fieldName
        }
        assertResult("appearsIn") {
          outputDefinitions(0).fields(1).fieldName
        }
      }

      it("should get the field types correctly") {
        assertResult(Required(GraphQLString)) {
          outputDefinitions(0).fields(0).fieldType
        }
        assertResult(Required(GraphQLList(GraphQLString))) {
          outputDefinitions(0).fields(1).fieldType
        }
      }
    }

    describe(
      "for a more complicated Star Wars example with multiple type definitions") {
      val schemaString = Source
        .fromURL(getClass.getResource("/CharactersAndEpisodes.graphql"))
        .mkString
      val outputDefinitions = parser.parse(schemaString)
      it("should extract the correct number of types") {
        assertResult(2) {
          outputDefinitions.length
        }
      }
      it("should extract the correct type names") {
        assertResult("Character") {
          outputDefinitions(0).definedType.name
        }
        assertResult("Episode") {
          outputDefinitions(1).definedType.name
        }
      }
    }
  }
}
