case class Token(typ: TokenType, lexeme: String, line: Int) {
  override def toString(): String = {
    return s"$typ <$lexeme>"
  }
}

enum TokenType:
  // structure
  case LeftParen, RightParen, LeftBrace, RightBrace
  case Comma, Semicolon
  // operators
  case Equal, Dot
  case Minus, Plus, Slash, Star, Bang
  case EqualEqual, BangEqual
  case Greater, GreaterEqual
  case Less, LessEqual
  case And, Or
  // keywords
  case Var, Fun, Class, This, Super
  case If, Else, For, While, Return
  // literals
  case True, False, Nil
  case NumberLiteral(value: Double)
  case StringLiteral(value: String)
  // identifiers
  case Print
  case Identifier(name: String)
  // extras
  case Comment(content: String)
  case WhiteSpace
  case EOF
