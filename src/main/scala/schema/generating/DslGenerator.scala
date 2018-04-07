package scalagraphqlclient.schema.generating
import scalagraphqlclient.schema.parsing._

abstract class Generator {
  def generate(types: Seq[TypeDefinition]): String
}

