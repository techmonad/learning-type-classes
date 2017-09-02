package com.techmonad.typeclasses.solution1


sealed trait Expr extends JsonConvertible

case class Number(n: Int) extends Expr {
  override def toJson = JNumber(n)
}

case class Add(expr1: Expr, expr2: Expr) extends Expr {
  override def toJson =
    JObject(List(("opt", JString("+")), ("left", expr1.toJson), ("right", expr2.toJson)))
}

case class Sub(expr1: Expr, expr2: Expr) extends Expr {
  override def toJson =
    JObject(List(("opt", JString("-")), ("left", expr1.toJson), ("right", expr2.toJson)))

}

case class Multiply(expr1: Expr, expr2: Expr) extends Expr {
  override def toJson =
    JObject(List(("opt", JString("*")), ("left", expr1.toJson), ("right", expr2.toJson)))

}

object ExprEvaluator{

  def eval(expr: Expr):Int=
    expr match {
      case Number(n) => n
      case Add(expr1,expr2) => eval(expr1) + eval(expr2)
      case Sub(expr1,expr2) => eval(expr1) - eval(expr2)
      case Multiply(expr1, expr2) => eval(expr1) * eval(expr2)
    }
}

object Solution1 extends App {

  val expr = Multiply(Number(1), Number(2))
  println("Expr result : " + ExprEvaluator.eval(expr))
  println("Json : " + JsonWriter.write(expr))

}