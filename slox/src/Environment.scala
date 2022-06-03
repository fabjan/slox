import scala.collection.mutable.HashMap

class Environment() {
  val values = new HashMap[String, LoxObject]()

  def define(name: String, value: LoxObject): Unit = {
    values.put(name, value)
  }

  def get(name: Token): LoxObject = {
    values.getOrElse(
      name.lexeme, {
        throw new RuntimeError(
          name,
          s"undefined variable '${name.lexeme}'",
        )
      },
    )
  }
}
