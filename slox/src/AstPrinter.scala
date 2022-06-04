class AstPrinter {

  def print(expr: Expr): String = expr match {
    case Expr.Binary(a, op, b) => parenthesize(op.lexeme, a, b)
    case Expr.Grouping(expr)   => parenthesize("group", expr)
    case Expr.Literal(v)       => if (v == null) then "nil" else s"$v"
    case Expr.Unary(op, expr)  => parenthesize(op.lexeme, expr)
    case Expr.Variable(v)      => v.lexeme
    case Expr.Assign(v, expr)  => parenthesize("=", Expr.Variable(v), expr)
  }

  def parenthesize(name: String, exprs: Expr*): String = {
    exprs.map(print).prepended(name).mkString("(", " ", ")")
  }
}

object AstPrinter {
  def test(): Unit = {
    // -123 * (45.67)
    val expr =
      Expr.Binary(
        Expr.Unary(
          Token(TokenType.Minus, "-", 1),
          Expr.Literal(123),
        ),
        Token(TokenType.Star, "*", 1),
        Expr.Grouping(Expr.Literal(45.67)),
      )

    println(new AstPrinter().print(expr))
  }
}
