import java.nio.file.Files
import java.nio.file.Path
import scala.io.StdIn.readLine

object Lox {

  var hadError = false

  def main(args: Array[String]): Unit = {
    args match {
      case Array(file) => runFile(file)
      case Array()     => runPrompt()
      case _           => usage()
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

    val tokens = scanner.scanTokens()

    tokens.foreach(println)
  }

  def error(line: Int, message: String): Unit = {
    report(line, "", message)
  }

  private def report(line: Int, where: String, message: String): Unit = {
    System.err.println("[line " + line + "] Error" + where + ": " + message)
    hadError = true
  }
}
