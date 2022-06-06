import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

import TokenType._

case class ParseError(message: String) extends RuntimeException

class Parser(tokens: List[Token]) {

  var current = 0

  def parse(): List[Stmt] = {
    val statements = new ArrayBuffer[Stmt]()

    while (!isAtEnd()) {
      declaration().foreach(statements.addOne)
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
    peek().typ match {
      case For       => forStatement()
      case If        => ifStatement()
      case Print     => printStatement()
      case While     => whileStatement()
      case LeftBrace => Stmt.Block(block())
      case _         => expressionStatement()
    }
  }

  private def forStatement(): Stmt = {
    consume(For, "interpreter error")

    consume(LeftParen, "Expect '(' after 'for'.")

    val initializer = if (munch(Semicolon).nonEmpty) {
      None
    } else if (check(Var)) {
      Some(varDeclaration())
    } else {
      Some(expressionStatement())
    }

    val condition = if (check(Semicolon)) {
      None
    } else {
      Some(expression())
    }
    consume(Semicolon, "Expect ';' after loop condition.")

    val increment = if (check(RightParen)) {
      None
    } else {
      Some(expression())
    }
    consume(RightParen, "Expect ')' after for clauses.")

    var loop = statement()

    increment.foreach((incr) => {
      loop = Stmt.Block(List(loop, Stmt.Expression(incr)))
    })

    loop = Stmt.While(condition.getOrElse(Expr.Literal(true)), loop)

    initializer.foreach((init) => {
      loop = Stmt.Block(List(init, loop))
    })

    loop
  }

  private def ifStatement(): Stmt = {
    consume(If, "interpreter error")

    consume(LeftParen, "Expect '(' after 'if'.")
    val condition = expression()
    consume(RightParen, "Expect ')' after if condition.")

    val thenBranch = statement()
    val elseBranch = if (munch(Else).nonEmpty) Some(statement()) else None

    Stmt.If(condition, thenBranch, elseBranch)
  }

  private def printStatement(): Stmt = {
    consume(Print, "interpreter error")
    val expr = expression()
    stmtEnd()
    Stmt.Print(expr)
  }

  private def whileStatement(): Stmt = {
    consume(While, "interpreter error")

    consume(LeftParen, "Expect '(' after 'while'.")
    val condition = expression()
    consume(RightParen, "Expect ')' after loop condition.")

    val body = statement()

    Stmt.While(condition, body)
  }

  private def block(): List[Stmt] = {
    val statements = new ArrayBuffer[Stmt]()
    consume(LeftBrace, "interpreter error")
    while (!check(RightBrace) && !isAtEnd()) {
      declaration().foreach(statements.addOne)
    }
    consume(RightBrace, "Expect '}' after block.")

    statements.toList
  }

  private def expressionStatement(): Stmt = {
    val expr = expression()
    stmtEnd()
    Stmt.Expression(expr)
  }

  private def stmtEnd(): Unit = {
    consume(Semicolon, "Expect ';' at end of statement.")
  }

  // public for test code and REPL
  def expression(): Expr = assignment()

  private def assignment(): Expr = {
    (or(), munch(Equal)) match {
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

  // Diverging from the book here by using the Binary AST node type since I
  // don't use the visitor pattern.
  private def or(): Expr =
    binary(and, Or)
  private def and(): Expr =
    binary(equality, And)

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

    call()
  }

  private def call(): Expr = {
    var expr = primary()

    var continue = true
    while (continue) {
      if (munch(LeftParen).nonEmpty) {
        expr = finishCall(expr)
      } else {
        continue = false
      }
    }

    expr
  }

  private def finishCall(callee: Expr): Expr = {
    val arguments = new ArrayBuffer[Expr]()

    if (!check(RightParen)) {
      var continue = true
      while (continue) {
        arguments.addOne(expression())
        if (munch(Comma).isEmpty) {
          continue = false
        }
      }
    }

    if (arguments.length >= 255) {
      error(peek(), "Can't have more than 255 arguments.")
    }

    val paren = consume(RightParen, "Expect ')' after arguments.")

    Expr.Call(callee, paren, arguments.toList)
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
      expr = new Expr.Binary(expr, operator, right)
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

  // public for REPL use
  def isAtEnd() =
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
    Lox.staticError(token, message)
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
