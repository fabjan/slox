import scala.collection.mutable.ArrayBuffer

class Scanner(source: String) {

  var start = 0
  var current = 0
  var line = 1
  val tokens = new ArrayBuffer[Token]()

  def scanTokens(): List[Token] = {
    import TokenType.{WhiteSpace, EOF}

    while (!isAtEnd()) {
      start = current
      scanToken()
        .filter(x => x.typ != WhiteSpace)
        .foreach(tokens.addOne)
    }

    tokens.addOne(Token(EOF, "", line))

    tokens.toList
  }

  private def isAtEnd(): Boolean = {
    source.length() <= current
  }

  private def peek(): Char = {
    if (isAtEnd()) {
      '\u0000'
    } else {
      source.charAt(current)
    }
  }

  private def peekNext(): Char = {
    if (source.length() <= current + 1) {
      '\u0000'
    } else {
      source.charAt(current + 1)
    }
  }

  private def advance(): Char = {
    val get = source.charAt(current)
    current += 1
    get
  }

  private def munch(expected: Char): Boolean = {
    if (isAtEnd()) {
      return false
    }
    if (source.charAt(current) != expected) {
      return false
    }

    current += 1
    true
  }

  private val isWhitespace: Char => Boolean = {
    case ' ' | '\r' | '\t' => true
    case _                 => false
  }

  private def isDigit(c: Char): Boolean = {
    '0' <= c && c <= '9'
  }

  private val isAlpha: Char => Boolean = {
    case c if 'a' <= c && c <= 'z' => true
    case c if 'A' <= c && c <= 'Z' => true
    case '_'                       => true
    case _                         => false
  }

  private def isAlnum(c: Char): Boolean = {
    isAlpha(c) || isDigit(c)
  }

  private def scanToken(): Option[Token] = {
    import TokenType._

    val c = advance()

    val typ = Some(c).collect({
      case '\n'                 => { line += 1; WhiteSpace }
      case c if isWhitespace(c) => space()
      case c if isDigit(c)      => number()
      case c if isAlpha(c)      => identifier()

      case '(' => LeftParen
      case ')' => RightParen
      case '{' => LeftBrace
      case '}' => RightBrace
      case ',' => Comma
      case ';' => Semicolon
      case '.' => Dot
      case '-' => Minus
      case '+' => Plus
      case '*' => Star
      case '=' => if (munch('=')) EqualEqual else Equal
      case '!' => if (munch('=')) BangEqual else Bang
      case '>' => if (munch('=')) GreaterEqual else Greater
      case '<' => if (munch('=')) LessEqual else Less
      case '/' => if (munch('/')) comment() else Slash
      case '"' => string()
    })

    if (typ.isEmpty) {
      Lox.error(line, s"Unexpected character: $c")
    }

    typ.map((t) => {
      val text = source.substring(start, current)
      Token(t, text, line)
    })
  }

  private def comment(): TokenType = {
    while (peek() != '\n' && !isAtEnd()) {
      advance()
    }
    val content = source.substring(start, current)
    TokenType.Comment(content.trim())
  }

  private def space(): TokenType = {
    while (isWhitespace(peek())) {
      advance()
    }
    TokenType.WhiteSpace
  }

  private def number(): TokenType = {
    while (isDigit(peek())) {
      advance()
    }
    if (peek() == '.' && isDigit(peekNext())) {
      advance()
      while (isDigit(peek())) {
        advance()
      }
    }
    TokenType.NumberLiteral(source.substring(start, current).toDouble)
  }

  private def string(): TokenType = {

    while (peek() != '"' && !isAtEnd()) {
      if (peek() == '\n') {
        line += 1
      }
      advance()
    }

    // Don't include the quotes.
    val value = source.substring(start + 1, current)

    if (isAtEnd()) {
      Lox.error(line, "Unterminated string: ")
    } else {
      // Consume the closing quote.
      advance()
    }

    // This means we return the unterminated string as well,
    // but I didn't want to complicate the Option.collect in scanToken,
    // and hopefully this works alright.
    TokenType.StringLiteral(value)
  }

  private def identifier(): TokenType = {
    import TokenType._

    while (isAlnum(peek())) {
      advance()
    }

    source.substring(start, current) match {
      case "and"    => And
      case "class"  => Class
      case "else"   => Else
      case "false"  => False
      case "for"    => For
      case "fun"    => Fun
      case "if"     => If
      case "nil"    => Nil
      case "or"     => Or
      case "print"  => Print
      case "return" => Return
      case "super"  => Super
      case "this"   => This
      case "true"   => True
      case "var"    => Var
      case "while"  => While
      case name     => Identifier(name)
    }
  }
}

object Scanner {
  def test(): Unit = {
    import TokenType._

    Expect.equal(
      "can scan bang",
      new Scanner("!").scanToken(),
      Some(Token(Bang, "!", 1)),
    )
    Expect.equal(
      "can scan numbers",
      new Scanner("1 2 3 4 5").scanTokens(),
      List(
        Token(NumberLiteral(1), "1", 1),
        Token(NumberLiteral(2), "2", 1),
        Token(NumberLiteral(3), "3", 1),
        Token(NumberLiteral(4), "4", 1),
        Token(NumberLiteral(5), "5", 1),
        Token(EOF, "", 1),
      ),
    )
  }
}
