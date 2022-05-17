import scala.collection.mutable.ListBuffer

class Scanner(source: String) {

  var start = 0
  var current = 0
  var line = 1
  val tokens = new ListBuffer[Token]()

  def scanTokens(): List[Token] = {

    while (!isAtEnd()) {
      start = current
      scanToken()
        .filter(x => x.typ != PlainToken.WhiteSpace)
        .foreach(tokens.addOne)
    }

    tokens.addOne(Token(PlainToken.EOF, "", line))

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
    import PlainToken._

    val c = advance()

    val typ = Some(c).collect({
      case '\n'                 => { line += 1; PlainToken.WhiteSpace }
      case c if isWhitespace(c) => space()
      case c if isDigit(c)      => number()
      case c if isAlpha(c)      => identifier()

      case '(' => LeftParen
      case ')' => RightParen
      case '{' => LeftBrace
      case '}' => RightBrace
      case ',' => Comma
      case '.' => Dot
      case ';' => Semicolon
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
    CommentToken(content.trim())
  }

  private def space(): TokenType = {
    while (isWhitespace(peek())) {
      advance()
    }
    PlainToken.WhiteSpace
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
    NumberToken(source.substring(start, current).toDouble)
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
    StringToken(value)
  }

  private def identifier(): TokenType = {
    while (isAlnum(peek())) {
      advance()
    }

    source.substring(start, current) match {
      case "and"    => PlainToken.And
      case "class"  => PlainToken.Class
      case "else"   => PlainToken.Else
      case "false"  => PlainToken.False
      case "for"    => PlainToken.For
      case "fun"    => PlainToken.Fun
      case "if"     => PlainToken.If
      case "nil"    => PlainToken.Nil
      case "or"     => PlainToken.Or
      case "print"  => PlainToken.Print
      case "return" => PlainToken.Return
      case "super"  => PlainToken.Super
      case "this"   => PlainToken.This
      case "true"   => PlainToken.True
      case "var"    => PlainToken.Var
      case "while"  => PlainToken.While
      case name     => IdentifierToken(name)
    }
  }
}
