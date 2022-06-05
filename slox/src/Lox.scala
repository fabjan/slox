import java.nio.file.Files
import java.nio.file.Path
import scala.io.StdIn.readLine

object Lox {

  val interpreter = new Interpreter()
  var hadError = false
  var hadRuntimeError = false
  var silenceErrors = false

  def main(args: Array[String]): Unit = {
    args match {
      case Array("test", what) => runTest(what)
      case Array(file)         => runFile(file)
      case Array()             => runPrompt()
      case _                   => usage()
    }
  }

  def usage(): Unit = {
    println("Usage: slox [script]")
    System.exit(64)
  }

  def runFile(path: String): Unit = {

    val source = Files.readString(Path.of(path))

    run(source)

    if (hadError) { System.exit(65) }
    if (hadRuntimeError) { System.exit(70) }
  }

  def runPrompt(): Unit = {

    print("> ")
    val line = readLine()
    if (line == null) {
      return
    }

    run(line, true)
    hadError = false

    runPrompt()
  }

  def run(source: String, inRepl: Boolean = false): Unit = {

    val scanner = new Scanner(source)

    val tokens = scanner
      .scanTokens()
      .filter({
        case Token(TokenType.Comment(_), _, _) => false
        case _                                 => true
      })

    // The REPL supports directly evaluating an expression and printing it.
    if (inRepl && !hadError) {
      try {
        silenceErrors = true
        val parser = new Parser(tokens)
        val expr = parser.expression()
        silenceErrors = false
        if (!hadError && parser.isAtEnd()) {
          return interpreter.interpret(List(Stmt.Print(expr)))
        }
      } catch {
        case _ => ()
      } finally {
        silenceErrors = false
        hadError = false
      }
    }

    val program = new Parser(tokens).parse()

    // Don't interpret if there was a syntax error.
    if (hadError) { return }

    interpreter.interpret(program)
  }

  def runTest(what: String): Unit = what match {
    case "ast-printer" => AstPrinter.test()
    case "rpn-printer" => RpnPrinter.test()
    case "scanner"     => Scanner.test()
    case "parser"      => Parser.test()
    case "interpreter" => Interpreter.test()
  }

  def staticError(line: Int, message: String): Unit = {
    hadError = true
    printError(line, None, message)
  }

  def staticError(token: Token, message: String): Unit = {
    hadError = true
    printError(token.line, Some(token), message)
  }

  def runtimeError(error: RuntimeError): Unit = {
    hadRuntimeError = true
    printError(error.token.line, Some(error.token), error.message)
  }

  private def printError(
      line: Int,
      token: Option[Token],
      message: String,
  ): Unit = {

    if (silenceErrors) { return }

    val where = token match {
      case Some(Token(TokenType.EOF, _, _)) => " at end"
      case Some(token)                      => s" at '${token.lexeme}'"
      case None                             => ""
    }
    println("[line " + line + "] Error" + where + ": " + message)
  }
}
