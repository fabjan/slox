class AstPrinter {

  def print(expr: Expr): String = expr match {
    case Expr.Binary(left, op, right) => parenthesize(op.lexeme, left, right)
    case Expr.Grouping(expr)          => parenthesize("group", expr)
    case Expr.Literal(v)              => if (v == null) then "nil" else s"$v"
    case Expr.Unary(op, expr)         => parenthesize(op.lexeme, expr)
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

    println(new AstPrinter().print(expr));
  }
}
