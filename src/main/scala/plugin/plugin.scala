import sbt._
import Keys._
import java.io.File
import plugins._

object ScalaGraphQLClientPlugin extends AutoPlugin {
  override def requires = JvmPlugin
  override lazy val projectSettings = Seq(
    unmanagedSourceDirectories in Compile += baseDirectory.value / "graphql",
    sourceGenerators in Compile += Def.task {
      def generate(input: Set[File]): Set[File] = Set((sourceManaged in Compile).value / "graphql_clients"/ "GraphQLClient.scala")
      val files = generate(Set()).toSeq
      IO.write(files(0), "")
      files
    }.taskValue
  )
}
