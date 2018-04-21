import sbt._
import Keys._

object ScalaGraphQLClientPlugin extends AutoPlugin {
  override lazy val projectSettings = Seq(commands += helloCommand)
  lazy val helloCommand =
    Command.command("hello") {( state: State ) => {
      println("Hi!")
      state
    }
    }
}
