package scalagraphqlclient.parsing

case class NamedQuery[T](name: String, format: T)

case class Query(name: String)
