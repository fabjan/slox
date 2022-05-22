import scala.util.Try

case class RuntimeError(token: Token, message: String) extends RuntimeException

// TODO: use case classes instead
type LoxObject = Any

class Interpreter {

  def interpret(statements: List[Stmt]): Unit = {
    try {
      statements.foreach(execute)
    } catch {
      case error: RuntimeError => Lox.runtimeError(error);
    }
  }

  private def stringify(obj: LoxObject): String = obj match {
    case null => "nil"
    case d: Double => {
      val text = s"$obj"
      if (text.endsWith(".0")) {
        text.substring(0, text.length() - 2)
      } else {
        text
      }
    }
    case default => s"$default"
  }

  private def execute(stmt: Stmt): Unit = stmt match {
    case Stmt.Expression(expr) => { evaluate(expr); () }
    case Stmt.Print(expr)      => { println(stringify(evaluate(expr))); () }
  }

  private def evaluate(expr: Expr): LoxObject = expr match {
    case Expr.Literal(v)              => v
    case Expr.Grouping(expr)          => evaluate(expr)
    case Expr.Unary(op, expr)         => unary(op, expr)
    case Expr.Binary(left, op, right) => binary(left, op, right)
  }

  private def unary(op: Token, expr: Expr): LoxObject = {

    import TokenType._

    val arg = evaluate(expr)

    (op.typ) match {
      case Bang => !loxTruthy(arg)
      case Minus => {
        checkNumberOperands(op, arg)
        0 - double(arg)
      }
      case _ => throw RuntimeError(op, "not a unary operator")
    }
  }

  private def binary(lexpr: Expr, op: Token, rexpr: Expr): LoxObject = {

    import TokenType._

    val left = evaluate(lexpr)
    val right = evaluate(rexpr)

    def isString(xs: LoxObject*): Boolean = xs.forall(_.isInstanceOf[String])

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
  ): T = {
    checkNumberOperands(op, left, right)
    primitive(double(left), double(right))
  }

  private def isDouble(args: LoxObject*): Boolean = {
    args.forall(_.isInstanceOf[Double])
  }

  private def checkNumberOperands(op: Token, args: LoxObject*): Unit = {
    if (!isDouble(args*)) {
      throw RuntimeError(op, s"operands must be numbers")
    }
  }

  private def double(x: LoxObject): Double = x.asInstanceOf[Double]

  private def loxTruthy(v: LoxObject): Boolean = v match {
    case null  => false
    case false => false
    case _     => true
  }

  private def loxEquals(a: LoxObject, b: LoxObject): Boolean = (a, b) match {
    case (null, null) => true
    case (null, _)    => false
    case (a, b)       => a.equals(b)
  }

  private def loxPlus(op: Token, a: LoxObject, b: LoxObject): LoxObject = {
    if (a.isInstanceOf[String] && b.isInstanceOf[String]) {
      s"$a$b"
    } else if (isDouble(a, b)) {
      double(a) + double(b)
    } else {
      throw RuntimeError(op, s"operands must be two numbers or two strings")
    }
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
