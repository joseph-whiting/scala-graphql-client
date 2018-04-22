package example
import graphqlclient.Client
import graphqlclient.Client._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.util.{Success, Failure}

object Hello extends App {
  val response = Client.send(query(
    character()(
      age
      name
    )
  ))
  val nameResponse = response.map(_.character.map(_.name))
  nameResponse onComplete {
    case Success(names: Seq[String]) => {
      val thoseWhoSayHello = names.mkString(" and ")
      println(s"Hello from ${thoseWhoSayHello}")
    }
    case _ => {
      println("Failed to find anyone saying hello")
    }
  }
}
