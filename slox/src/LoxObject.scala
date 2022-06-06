sealed trait LoxObject

enum LoxThing extends LoxObject:
  case LoxNumber(value: Double)
  case LoxBoolean(value: Boolean)
  case LoxString(value: String)
  case LoxNil

trait LoxCallable extends LoxObject {
  val arity: Int
  def call(interpreter: Interpreter, arguments: List[LoxObject]): LoxObject
}

class LoxFunction(declaration: Stmt.Function) extends LoxCallable {

  override def toString(): String = {
    s"<fn ${declaration.name.lexeme}>"
  }

  val arity = declaration.params.length

  def call(interpreter: Interpreter, arguments: List[LoxObject]): LoxObject = {
    // functions can only be defined in the global scope, so this should be fine
    val env = interpreter.globals.newScope()
    arguments.zipWithIndex.foreach((arg, i) => {
      env.define(declaration.params(i).lexeme, arguments(i))
    })

    interpreter.executeBlock(declaration.body, env)
    LoxThing.LoxNil
  }
}
