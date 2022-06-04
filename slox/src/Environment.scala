import scala.collection.mutable.HashMap

class Environment() {
  val values = new HashMap[String, LoxObject]()

  def define(name: String, value: LoxObject): Unit = {
    values.put(name, value)
  }

  def checkDefined(name: Token): Unit = {
    if (!values.contains(name.lexeme)) {
      throw new RuntimeError(name, s"Undefined variable '${name.lexeme}'.")
    }
  }

  def get(name: Token): LoxObject = {
    checkDefined(name)
    values(name.lexeme)
  }

  def assign(name: Token, value: LoxObject): LoxObject = {
    checkDefined(name)
    values.put(name.lexeme, value)
    value
  }
}
