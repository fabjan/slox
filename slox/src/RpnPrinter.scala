class RpnPrinter {

  def print(expr: Expr): String = expr match {
    case Expr.Binary(a, op, b) => revpol(Some(op.lexeme), a, b)
    case Expr.Grouping(expr)   => revpol(None, expr)
    case Expr.Literal(v)       => if (v == null) then "nil" else s"$v"
    case Expr.Unary(op, expr)  => revpol(Some(op.lexeme), expr)
    case Expr.Variable(v)      => v.lexeme
    case Expr.Assign(v, expr)  => revpol(Some("="), expr, Expr.Variable(v))
  }

  def revpol(op: Option[String], exprs: Expr*): String = {
    exprs.map(print).concat(op).mkString(" ")
  }
}

object RpnPrinter {
  def test(): Unit = {
    // (1 + 2) * (4 - 3)
    val expr = Expr.Binary(
      Expr.Grouping(
        Expr.Binary(
          Expr.Literal(1),
          Token(TokenType.Plus, "+", 1),
          Expr.Literal(2),
        ),
      ),
      Token(TokenType.Star, "*", 1),
      Expr.Grouping(
        Expr.Binary(
          Expr.Literal(4),
          Token(TokenType.Minus, "-", 1),
          Expr.Literal(3),
        ),
      ),
    )

    println("expect: 1 2 + 4 3 - *")
    println("result: " + new RpnPrinter().print(expr))
  }
}
