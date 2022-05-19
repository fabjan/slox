/**
 * expression     → literal
 *                | unary
 *                | binary
 *                | grouping ;
 * 
 * literal        → NUMBER | STRING | "true" | "false" | "nil" ;
 * grouping       → "(" expression ")" ;
 * unary          → ( "-" | "!" ) expression ;
 * binary         → expression operator expression ;
 * operator       → "==" | "!=" | "<" | "<=" | ">" | ">="
 *                | "+"  | "-"  | "*" | "/" ;
 */

enum Expr:
  case Literal(value: Any)
  case Grouping(expr: Expr)
  case Unary(op: Token, expr: Expr)
  case Binary(left: Expr, op: Token, right: Expr)
