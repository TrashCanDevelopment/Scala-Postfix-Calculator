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

  sealed trait Expression
  case class NumberExpression(value: Int) extends Expression
  case class OperationExpression(lhs: Expression, rhs: Expression, op: Operator) extends Expression

  /*
  * Parse Postfix
  * */
  def parse(expression: String): Expression = {
    val stack = new Stack[Expression]

    for (token <- expression.split(" ")) token match {
      case Number(num) => stack.push(NumberExpression(num))
      case Operator(op) =>
        val rhs = stack.pop()
        val lhs = stack.pop()
        stack.push(OperationExpression(lhs, rhs, op))
      case _ => throw new IllegalArgumentException("invalid token: " + token)
    }
    stack.pop() // return
  }

  def calculate(expression: Expression): Int = expression match {
    case NumberExpression(value) => value
    case OperationExpression(lhs, rhs, op) => op.operate(calculate(lhs), calculate(rhs))
  }

  def toInfix(expression: Expression): String = expression match {
    case NumberExpression(value) => value.toString()
    case OperationExpression (lhs, rhs, op) => s"(${toInfix(lhs)} $op ${toInfix(rhs)})"
  }

  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      throw new IllegalArgumentException("Usage: Calculator <expression>") //run "1 1 + 5 +" // 7
    } else {
      val expression = parse(args(0))
      println(s"${toInfix(expression)} = ${calculate(expression)}")
    }
  }
}