// format: off
/**
 * program        → statement* EOF ;
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
 * primary        → NUMBER | STRING | "true" | "false" | "nil"
 *                | "(" expression ")" ;
 */
// format: on

enum Expr:
  case Literal(value: Any)
  case Grouping(expr: Expr)
  case Unary(op: Token, expr: Expr)
  case Binary(left: Expr, op: Token, right: Expr)

enum Stmt:
  case Expression(expr: Expr)
  case Print(expr: Expr)
