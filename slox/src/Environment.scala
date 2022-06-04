import scala.collection.mutable.HashMap

class Environment(enclosing: Option[Environment] = None) {

  val values = new HashMap[String, LoxObject]()

  def define(name: String, value: LoxObject): Unit = {
    values.put(name, value)
  }

  def get(name: Token): LoxObject = {
    val found = values
      .get(name.lexeme)
      .orElse(enclosing.map(_.get(name)))

    if (found.nonEmpty) {
      return found.get
    }

    throw new RuntimeError(name, s"Undefined variable '${name.lexeme}'.")
  }

  def assign(name: Token, value: LoxObject): LoxObject = {
    if (values.contains(name.lexeme)) {
      values.put(name.lexeme, value)
      return value
    }

    if (enclosing.nonEmpty) {
      enclosing.foreach(_.assign(name, value))
      return value
    }

    throw new RuntimeError(name, s"Undefined variable '${name.lexeme}'.")
  }
}
