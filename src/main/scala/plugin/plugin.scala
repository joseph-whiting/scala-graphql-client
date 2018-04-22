import sbt._
import Keys._
import java.io.File
import plugins._
import scalagraphqlclient.schema.generating.DslGenerator
import scalagraphqlclient.schema.parsing.SchemaParser

object ScalaGraphQLClientPlugin extends AutoPlugin {
  override def requires = JvmPlugin
  val extensionToGraphQLSchemas = "src/main/graphql/schema.graphql"
  override lazy val projectSettings = Seq(
    unmanagedSourceDirectories in Compile += baseDirectory.value / extensionToGraphQLSchemas,
    sourceGenerators in Compile += Def.task {
      val inputFile = baseDirectory.value / extensionToGraphQLSchemas
      val outputFile = (sourceManaged in Compile).value / "graphql_clients"/ "GraphQLClient.scala"
      val dslGenerator = new DslGenerator()
      val schemaParser = new SchemaParser()
      val inputSchema = IO.read(inputFile)
      val types = schemaParser.parse(inputSchema)
      val dsl = dslGenerator.generate(types)
      IO.write(outputFile, "package graphqlclient\n".concat(dsl))
      Seq(outputFile)
    }.taskValue
  )
}
