import scala.collection.mutable.Stack

object Calculator {
  /**
    * if the token is an operator, pop 2 operands off the stack,
    * perform the operation and push the result back on
    */
  def handleOperator(token: String, stack: Stack[Int]): Boolean = token match {
    case "+" =>
      val rhs = stack.pop()
      val lhs = stack.pop()
      stack.push(lhs + rhs)
      true
    case "-" =>
      val rhs = stack.pop()
      val lhs = stack.pop()
      stack.push(lhs - rhs)
      true
    case "*" =>
      val rhs = stack.pop()
      val lhs = stack.pop()
      stack.push(lhs * rhs)
      true
    case "/" =>
      val rhs = stack.pop()
      val lhs = stack.pop()
      stack.push(lhs / rhs)
      true
    case _ => false
  }


  /*
  * if the token is a number, push it on the stack
  * */
  def handleNumber(token: String, stack: Stack[Int]): Boolean = try {
    stack.push(token.toInt)
    true
  } catch {
    case _: NumberFormatException => false
  }

  def calculate(expression: String): Int = {
    val stack = new Stack[Int] // kind of state

    for (token <- expression.split(" "))
      if (!handleOperator(token, stack) && !handleNumber(token, stack))
        throw new IllegalArgumentException("Invalid token: " + token)
    stack.pop()
  }

  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      throw new IllegalArgumentException("Usage: Calculator <expression>")
    } else {
      println(calculate(args(0)))
    }
  }
}