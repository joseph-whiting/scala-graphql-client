import org.scalatest._
import scalagraphqlclient.schema.converting._
import scalagraphqlclient.schema.parsing._

class ConverterSpec extends FunSpec {
  describe("the converter from graphQL models (the results of parsing) to the internal model (the input for code generator and describing the way that users will see the model)") {
    describe("for character type with non-required age, required name, and a list of favourite drinks") {
      val graphQLModel: Seq[TypeDefinition] = Seq(
        TypeDefinition(
          DefinedType("Character"),
          Seq(
            Field("name", Required(GraphQLString)),
            Field("age", GraphQLInt),
            Field("favourite_drinks", Required(GraphQLList(Required(GraphQLString))))
          )
        )
      )
      describe("the output model") {
        val output = GraphQLtoInternalConverter.convert(graphQLModel)
        it("should have the name field as a required string") {
          assert(output(0).fields(0).fieldType == InternalString)
        }
        it("should have the age field as an optional int") {
          assert(output(0).fields(1).fieldType == InternalOption(InternalInt))
        }
        it("should have the favourite drinks fields as a sequence of strings") {
          assert(output(0).fields(2).fieldType == InternalSeq(InternalString))
        }
      }
    }
  }
}
