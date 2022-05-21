import scala.util.{Try, Success, Failure}

import TokenType._

case class ParseError(message: String) extends RuntimeException

class Parser(tokens: List[Token]) {

  var current = 0;

  def parse(): Try[Expr] = {
    Try({ expression() })
  }

  private def expression(): Expr = equality()

  private def equality(): Expr =
    binary(comparison, BangEqual, EqualEqual)

  private def comparison(): Expr =
    binary(term, Greater, GreaterEqual, Less, LessEqual)

  private def term(): Expr =
    binary(factor, Minus, Plus)

  private def factor(): Expr =
    binary(unary, Slash, Star)

  private def unary(): Expr = {
    val t = munch(Bang, Minus)

    if (t.nonEmpty) {
      return Expr.Unary(t.get, unary())
    }

    primary()
  }

  private def primary(): Expr = {
    next().typ match {
      case False            => Expr.Literal(false)
      case True             => Expr.Literal(true)
      case Nil              => Expr.Literal(null)
      case NumberLiteral(n) => Expr.Literal(n)
      case StringLiteral(s) => Expr.Literal(s)
      case LeftParen => {
        val expr = expression()
        consume(RightParen, "Expect ')' after expression.")
        Expr.Grouping(expr)
      }
      case _ => throw error(peek(), "Expect expression.")
    }
  }

  private def binary(higher: () => Expr, ops: TokenType*): Expr = {
    var expr = higher()
    while (munch(ops*).nonEmpty) {
      val operator = previous()
      val right = higher()
      expr = new Expr.Binary(expr, operator, right);
    }
    expr
  }

  private def munch(types: TokenType*) =
    types.find(check).map(_ => next())

  private def consume(typ: TokenType, message: String): Token = {
    munch(typ).getOrElse {
      throw error(peek(), message)
    }
  }

  private def isAtEnd() =
    peek().typ == TokenType.EOF

  private def check(typ: TokenType) =
    !isAtEnd() && peek().typ == typ

  private def peek() =
    tokens(current)

  private def previous() =
    tokens(current - 1)

  private def next(): Token = {
    if (!isAtEnd()) {
      current += 1
    }
    previous()
  }

  private def error(token: Token, message: String): ParseError = {
    Lox.error(token, message)
    ParseError(message)
  }

  private def synchronize(): Unit = {
    next()

    while (!isAtEnd()) {
      if (previous().typ == Semicolon) return
      munch(Class, Fun, Var, For, If, While, Print, Return) match {
        case Some(_) => return
        case None    => next()
      }
    }
  }
}

object Parser {
  def test(): Unit = {
    import TokenType._

    def scan(source: String): List[Token] = new Scanner(source).scanTokens()

    Expect.equal(
      "can parse number literal",
      new Parser(scan("4711")).parse(),
      Success(Expr.Literal(4711.0)),
    )
    Expect.equal(
      "can parse string literal",
      new Parser(scan("\"goober\"")).parse(),
      Success(Expr.Literal("goober")),
    )
    Expect.equal(
      "can parse binary operation",
      new Parser(scan("1 + 2")).parse(),
      Success(
        Expr.Binary(Expr.Literal(1.0), Token(Plus, "+", 1), Expr.Literal(2.0)),
      ),
    )
  }
}
