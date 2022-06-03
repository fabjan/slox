// format: off
/**
 * program        → declaration* EOF ;
 *
 * declaration    → varDecl
 *                | statement ;
 * varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;
 *
 * statement      → exprStmt
 *                | printStmt ;
 * exprStmt       → expression ";" ;
 * printStmt      → "print" expression ";" ;
 * 
 * expression     → equality ;
 * equality       → comparison ( ( "!=" | "==" ) comparison )* ;
 * comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
 * term           → factor ( ( "-" | "+" ) factor )* ;
 * factor         → unary ( ( "/" | "*" ) unary )* ;
 * unary          → ( "!" | "-" ) unary
 *                | primary ;
 * primary        → "true" | "false" | "nil"
 *                | NUMBER | STRING
 *                | "(" expression ")"
 *                | IDENTIFIER ;
 */
// format: on

enum Expr:
  case Literal(value: Any)
  case Grouping(expr: Expr)
  case Unary(op: Token, expr: Expr)
  case Binary(left: Expr, op: Token, right: Expr)
  case Variable(name: Token)

enum Stmt:
  case Expression(expr: Expr)
  case Print(expr: Expr)
  case Var(name: Token, initializer: Option[Expr])
