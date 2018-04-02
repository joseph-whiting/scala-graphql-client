package scalagraphqlclient.schema.parsing

import org.scalatest._

class SchemaParserSpec extends FunSpec {
  describe("The schema parser") {
    val parser = new SchemaParser()
    describe("for a simple Star Wars character example") {
      val outputTypes = parser.parse(
        """type Character {""" +
        """  name: String!""" +
        """  appearsIn: [String]!""" +
        """}"""
      )

      it("should get the correct name for the top level type") {
        assertResult("Character") {
          outputTypes(0).name
        }
      }

      it("should get the right number of fields") {
        assertResult(2) {
          outputTypes(0).fields.length
        }
      }

      it("should get the names of the fields correctly") {
        assertResult("name") {
          outputTypes(0).fields(0).fieldName
        }
        assertResult("appearsIn") {
          outputTypes(0).fields(1).fieldName
        }
      }

      it("should get the field types correctly") {
        assertResult(Required(GraphQLString)) {
          outputTypes(0).fields(0).fieldType
        }
        assertResult(Required(GraphQLList(GraphQLString))) {
          outputTypes(0).fields(1).fieldType
        }
      }
    }
  }
}
