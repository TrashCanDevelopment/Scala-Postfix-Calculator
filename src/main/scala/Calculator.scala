import scala.collection.mutable.Stack

object Calculator {

  trait Operator {
    def operate(lhs: Int, rhs: Int): Int
  }
  object Operator {
    val operators: Map[String, Operator] =
      Map("+" -> Add, "-" -> Subtract, "*" -> Multiply, "/" -> Divide)
    def unapply(token: String): Option[Operator] =
      operators.get(token)
  }
  case object Add extends Operator {
    def operate(lhs: Int, rhs: Int): Int = lhs + rhs
    override val toString = "+"
  }
  case object Subtract extends Operator {
    def operate(lhs: Int, rhs: Int): Int = lhs - rhs
    override val toString = "-"
  }
  case object Multiply extends Operator {
    def operate(lhs: Int, rhs: Int): Int = lhs * rhs
    override val toString = "*"
  }
  case object Divide extends Operator {
    def operate(lhs: Int, rhs: Int): Int = lhs / rhs
    override val toString = "/"
  }

  object Number {
    def unapply(token: String): Option[Int] = try {
      Some(token.toInt)
    } catch {
      case _:NumberFormatException => None
    }
  }

  def calculate(expression: String): Int = {
    val stack = new Stack[Int]

    for (token <- expression.split(" ")) token match {
      case Number(num) => stack.push(num)
      case Operator(op) =>
        val rhs = stack.pop()
        val lhs = stack.pop()
        stack.push(op.operate(lhs, rhs))
      case _ => throw new IllegalArgumentException("invalid token: " + token)
    }
    stack.pop() // return
  }

  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      throw new IllegalArgumentException("Usage: Calculator <expression>") //run "1 1 + 5 +" // 7
    } else {
      println(calculate(args(0)))
    }
  }
}