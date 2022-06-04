import scala.util.Try

case class RuntimeError(token: Token, message: String) extends RuntimeException

enum LoxObject:
  case LoxNumber(value: Double)
  case LoxBoolean(value: Boolean)
  case LoxString(value: String)
  case LoxNil

import LoxObject._

import scala.language.implicitConversions
implicit def loxBool(x: Boolean): LoxObject = LoxBoolean(x)
implicit def loxNumber(x: Double): LoxObject = LoxNumber(x)
implicit def loxString(x: String): LoxObject = LoxString(x)

class Interpreter {

  var environment = new Environment()

  def interpret(statements: List[Stmt]): Unit = {
    try {
      statements.foreach(execute)
    } catch {
      case error: RuntimeError => Lox.runtimeError(error);
    }
  }

  private def stringify(obj: LoxObject): String = obj match {
    case LoxNil        => "nil"
    case LoxBoolean(b) => s"$b"
    case LoxString(s)  => s
    case LoxNumber(x) => {
      val text = s"$x"
      if (text.endsWith(".0")) {
        text.substring(0, text.length() - 2)
      } else {
        text
      }
    }
  }

  private def execute(stmt: Stmt): Unit = stmt match {
    case Stmt.Expression(expr) => {
      evaluate(expr)
      ()
    }
    }
    case Stmt.Print(expr) => {
      loxPrint(expr)
      ()
    }
    case Stmt.Var(i, init) => {
      init
        .map(evaluate)
        .foreach((v: LoxObject) => {
          environment.define(i.lexeme, v)
        })
    }
    case Stmt.Block(statements) => {
      executeBlock(statements, environment.newScope())
    }
  }

  private def executeBlock(
      statements: List[Stmt],
      scope: Environment,
  ): Unit = {
    val before = this.environment
    try {
      this.environment = scope
      statements.foreach(execute)
    } finally {
      this.environment = before
    }
  }

  private def evaluate(expr: Expr): LoxObject = expr match {
    case Expr.Literal(v: Boolean) => v
    case Expr.Literal(v: Double)  => v
    case Expr.Literal(v: String)  => v
    case Expr.Literal(null)       => LoxNil
    case Expr.Variable(v)         => environment.get(v)
    case Expr.Grouping(expr)      => evaluate(expr)
    case Expr.Binary(a, op, b)    => binary(a, op, b)
    case Expr.Unary(op, expr)     => unary(op, expr)
    case Expr.Assign(name, expr)  => environment.assign(name, evaluate(expr))

    case Expr.Literal(_) => throw RuntimeError(null, "this cannot happen")
  }

  private def unary(op: Token, expr: Expr): LoxObject = {

    import TokenType._

    val arg = evaluate(expr)

    (op.typ, arg) match {
      case (Bang, _)             => !loxTruthy(arg)
      case (Minus, LoxNumber(x)) => 0 - x
      case (Minus, _) => throw RuntimeError(op, "operand must be a number")
      case _          => throw RuntimeError(op, "not a unary operator")
    }
  }

  private def binary(lexpr: Expr, op: Token, rexpr: Expr): LoxObject = {

    import TokenType._

    val left = evaluate(lexpr)
    val right = evaluate(rexpr)

    (op.typ) match {

      case Plus  => loxPlus(op, left, right)
      case Minus => binaryNumberOp(op, left, right, (_ - _))
      case Slash => binaryNumberOp(op, left, right, (_ / _))
      case Star  => binaryNumberOp(op, left, right, (_ * _))

      case Greater      => binaryNumberOp(op, left, right, (_ > _))
      case GreaterEqual => binaryNumberOp(op, left, right, (_ >= _))
      case Less         => binaryNumberOp(op, left, right, (_ < _))
      case LessEqual    => binaryNumberOp(op, left, right, (_ <= _))

      case EqualEqual => loxEquals(left, right)
      case BangEqual  => !loxEquals(left, right)

      case _ => throw RuntimeError(op, "not a binary operator")
    }
  }

  private def binaryNumberOp[T](
      op: Token,
      left: LoxObject,
      right: LoxObject,
      primitive: (a: Double, b: Double) => T,
  ): T = (left, right) match {
    case (LoxNumber(a), LoxNumber(b)) => primitive(a, b)
    case _ => {
      throw RuntimeError(op, s"operands must be numbers")
    }
  }

  private def loxTruthy(v: LoxObject): Boolean = v match {
    case LoxBoolean(false) => false
    case LoxNil            => false
    case _                 => true
  }

  private def loxEquals(a: LoxObject, b: LoxObject): Boolean = (a, b) match {
    case (LoxNil, LoxNil) => true
    case (LoxNil, _)      => false
    case (a, b)           => a.equals(b)
  }

  private def loxPlus(op: Token, a: LoxObject, b: LoxObject): LoxObject =
    (a, b) match {
      case (LoxString(x), LoxString(y)) => s"$x$y"
      case (LoxNumber(x), LoxNumber(y)) => x + y
      case _ => {
        throw RuntimeError(op, s"operands must be two numbers or two strings")
      }
    }

  // exposed for REPL
  def loxPrint(expr: Expr): Unit = {
    println(stringify(evaluate(expr)))
  }
}

object Interpreter {
  def test(): Unit = {

    val interp = new Interpreter()

    def eval(source: String): LoxObject =
      interp.evaluate(Parser(new Scanner(source).scanTokens()).expression())

    Expect.equal(
      "can evaluate numbers",
      eval("1"),
      1.0,
    )

    Expect.equal(
      "can evaluate bang",
      eval("!0"),
      false,
    )

    Expect.equal(
      "can evaluate bang bang",
      eval("!!true"),
      true,
    )

    Expect.equal(
      "can add numbers",
      eval("47 + 11"),
      58.0,
    )

    Expect.equal(
      "can subtract numbers",
      eval("47 - 11"),
      36.0,
    )

    Expect.equal(
      "can multiply numbers",
      eval("47 * 11"),
      517.0,
    )

    Expect.equal(
      "can divide numbers",
      eval("47 / 11"),
      47.0 / 11.0,
    )

    Expect.equal(
      "can compare numbers",
      List(eval("1 < 2"), eval("1 <= 1"), eval("1 > 0"), eval("2 >= 2")),
      List(true, true, true, true),
    )

    Expect.equal(
      "can compare numbers 2",
      List(eval("1 < 1"), eval("2 <= 1"), eval("0 > 1"), eval("1 >= 2")),
      List(false, false, false, false),
    )

    Expect.equal(
      "can test equality",
      List(
        eval("1 == 1"),
        eval("nil == nil"),
        eval("\"foo\" == \"foo\""),
        eval("false == false"),
      ),
      List(true, true, true, true),
    )

    Expect.equal(
      "can add strings",
      eval("\"foo\" + \"bar\""),
      "foobar",
    )
  }
}
