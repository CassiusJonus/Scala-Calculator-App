package jonus.cassius.calculator

/**
 * This object is the entry point of the application.
 *
 * @author Cassius Jonus
 */
object Main {
  def main(args: Array[String]): Unit = {
    println {
      Calculator.evaluate("1+2*55") match {
        case Some(result) => s"The answer is $result"
        case None =>
      }
    }
  }
}






