package scalagraphqlclient.schema.generating
import scalagraphqlclient.schema.parsing._

class Generator {
  def generate(types: Seq[TypeDefinition]): String = """object WithName {
  implicit def innerObject[T](outer: TypedWithName[T]): T = outer.inner
  def ::[T](typed: T) = new TypedWithName[T](typed)
  class TypedWithName[T] (val inner: T) extends Name
}"""
}
