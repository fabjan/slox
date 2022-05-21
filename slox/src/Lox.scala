import java.nio.file.Files
import java.nio.file.Path
import scala.io.StdIn.readLine

object Lox {

  var hadError = false

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

    if (hadError) {
      System.exit(65)
    }
  }

  def runPrompt(): Unit = {

    print("> ")
    val line = readLine()
    if (line == null) {
      return
    }

    run(line)
    hadError = false

    runPrompt()
  }

  def run(source: String): Unit = {

    val scanner = new Scanner(source)

    val tokens = scanner
      .scanTokens()
      .filter({
        case Token(TokenType.Comment(_), _, _) => false
        case _                                 => true
      })

    val parser = new Parser(tokens)

    val expression = parser.parse()

    // Stop if there was a syntax error.
    if (hadError) { return }

    println(new AstPrinter().print(expression.get))
  }

  def runTest(what: String): Unit = what match {
    case "ast-printer" => AstPrinter.test()
    case "rpn-printer" => RpnPrinter.test()
    case "scanner"     => Scanner.test()
    case "parser"      => Parser.test()
  }

  def error(line: Int, message: String): Unit = {
    report(line, "", message)
  }

  def error(token: Token, message: String): Unit = {
    if (token.typ == TokenType.EOF) {
      report(token.line, " at end", message);
    } else {
      report(token.line, " at '" + token.lexeme + "'", message);
    }
  }

  private def report(line: Int, where: String, message: String): Unit = {
    System.err.println("[line " + line + "] Error" + where + ": " + message)
    hadError = true
  }
}
