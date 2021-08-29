package jonus.cassius.calculator

import scala.util.parsing.combinator.*

/**
 * This object contains methods for the Calculator.
 */
object Calculator extends JavaTokenParsers {
  /**
   * Evaluates the given expression from and returns an Option[Int]
   *
   * The current implementation evalutes expressions from right to left
   *
   * @param expr the expression to evaluate
   * @return an Option[Int] that may contain a result depending on whether the expression is successfully parsed.
   */
  def evaluate(expr: String): Option[Int] = parse(expression, expr) match {
    case Success(result, _) => Some(result)
    case Failure(msg, _) => println(s"Failed to parse expression:\n\n$msg"); None
    case Error(msg, _) => println(s"An error occured while attempting to parse the expression:\n\n$msg"); None
  }

  private def operator: Parser[String] = "+" | "-" | "*" | "/" ^^ {
    _.toString
  }

  private def expression: Parser[Int] = (wholeNumber ^^ {
    _.toInt
  }) ~ opt(operator ~ expression) ^^ {
    case a ~ None => a
    case a ~ Some("*" ~ b) => a * b
    case a ~ Some("/" ~ b) if b != 0 => a / b
    case a ~ Some("+" ~ b) => a + b
    case a ~ Some("-" ~ b) => a - b
  }


}
