import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

import TokenType._

case class ParseError(message: String) extends RuntimeException

class Parser(tokens: List[Token]) {

  var current = 0

  def parse(): List[Stmt] = {
    val statements = new ArrayBuffer[Stmt]()
    while (!isAtEnd()) {
      declaration().foreach(s => {
        statements.addOne(s)
      })
    }

    statements.toList
  }

  private def declaration(): Option[Stmt] = {
    Try {
      peek() match {
        case Token(Var, _, _) => varDeclaration()
        case _                => statement()
      }
    } match {
      case Success(stmt) => Some(stmt)
      case Failure(e: ParseError) => {
        synchronize()
        None
      }
      case Failure(e) => {
        throw e
      }
    }
  }

  private def varDeclaration(): Stmt = {
    consume(Var, "interpreter error")
    val name = peek() match {
      case Token(Identifier(_), _, _) => next()
      case t => throw (error(t, "expect variable name"))
    }
    val initializer = munch(Equal).map(_ => expression())
    consume(Semicolon, "expect ';' after variable declaration")
    Stmt.Var(name, initializer)
  }

  private def statement(): Stmt = {
    peek() match {
      case Token(Print, _, _) => printStatement()
      case _                  => expressionStatement()
    }
  }

  private def printStatement(): Stmt = {
    consume(Print, "interpreter error")
    val expr = expression()
    stmtEnd()
    Stmt.Print(expr)
  }

  private def expressionStatement(): Stmt = {
    val expr = expression()
    stmtEnd()
    Stmt.Expression(expr)
  }

  private def stmtEnd(): Unit = {
    consume(Semicolon, "Expect ';' at end of statement.")
  }

  // public for test code
  def expression(): Expr = assignment()

  private def assignment(): Expr = {
    (equality(), munch(Equal)) match {
      case (expr, None) => expr
      case (Expr.Variable(v), Some(_)) => {
        // right associativity
        val value = assignment()
        Expr.Assign(v, value)
      }
      case (left, Some(equals)) => {
        error(equals, "Invalid assignment target.")
        left
      }
    }
  }

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
      case Identifier(i)    => Expr.Variable(previous())
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
    // left associativity
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
    peek().typ == EOF

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

    def scan(source: String): List[Token] = new Scanner(source).scanTokens()

    Expect.equal(
      "can parse number literal",
      new Parser(scan("4711")).expression(),
      Expr.Literal(4711.0),
    )
    Expect.equal(
      "can parse string literal",
      new Parser(scan("\"goober\"")).expression(),
      Expr.Literal("goober"),
    )
    Expect.equal(
      "can parse binary operation",
      new Parser(scan("1 + 2")).expression(),
      Expr.Binary(Expr.Literal(1.0), Token(Plus, "+", 1), Expr.Literal(2.0)),
    )

    Expect.equal(
      "can parse expression statements",
      new Parser(scan("1 + 2;")).parse(),
      List(
        Stmt.Expression(
          Expr.Binary(Expr.Literal(1.0), Token(Plus, "+", 1), Expr.Literal(2.0)),
        ),
      ),
    )
    Expect.equal(
      "can parse print statements",
      new Parser(scan("print \"goober\";")).parse(),
      List(
        Stmt.Print(Expr.Literal("goober")),
      ),
    )
  }
}
