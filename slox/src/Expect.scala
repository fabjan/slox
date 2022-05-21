object Expect {

  def apply(message: String, cond: Boolean, explain: String): Unit = {

    val result = if (cond) "✔️" else "❌"

    val explanation = Some(explain).filterNot(_ => cond).getOrElse("")

    println(s"$message: $result $explanation")
  }

  def equal[T](message: String, actual: T, expected: T): Unit = {
    apply(
      message,
      expected == actual,
      s"\nexpected: $expected\nbut got : $actual",
    )
  }
}
