case class Token(typ: TokenType, lexeme: String, line: Int) {
  override def toString(): String = {
    return s"$typ <$lexeme>"
  }
}

sealed abstract class TokenType

case class IdentifierToken(name: String) extends TokenType
case class StringToken(value: String) extends TokenType
case class NumberToken(value: Double) extends TokenType
case class CommentToken(content: String) extends TokenType

enum PlainToken extends TokenType:
  case LeftParen, RightParen, LeftBrace, RightBrace
  case Comma, Dot, Semicolon
  case Minus, Plus, Slash, Star
  case Equal, EqualEqual
  case Bang, BangEqual
  case Greater, GreaterEqual
  case Less, LessEqual
  case And, Or
  case Var, Fun, Class
  case If, Else, For, While, Return
  case This, True, False, Nil
  case Print, Super
  case WhiteSpace
  case EOF
