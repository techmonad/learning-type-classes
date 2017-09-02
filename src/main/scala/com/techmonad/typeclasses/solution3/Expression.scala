package com.techmonad.typeclasses.solution3

sealed trait Expr

case class Number(n:Int) extends Expr
case class Add(expr1:Expr, expr2: Expr ) extends Expr
case class Sub(expr1: Expr, expr2: Expr ) extends Expr
case class Multiply(expr1: Expr, expr2: Expr ) extends Expr

object ExprEvaluator{

  def eval(expr: Expr):Int=
    expr match {
      case Number(n) => n
      case Add(expr1,expr2) => eval(expr1) + eval(expr2)
      case Sub(expr1,expr2) => eval(expr1) - eval(expr2)
      case Multiply(expr1, expr2) => eval(expr1) * eval(expr2)
    }
}



object Solution3 extends App {

  //type class instance
 implicit val jsonConverter = new JsonConverter[Expr]{
    override def convert(value: Expr) =
      value match {
        case Number(n) => JNumber(n)
        case Add(expr1,expr2) =>
          JObject(List(("opt", JString("+")), ("left", convert(expr1)), ("right", convert(expr2))))

        case Sub(expr1,expr2) =>
          JObject(List(("opt", JString("-")), ("left", convert(expr1)), ("right", convert(expr2))))

        case Multiply(expr1,expr2) =>
          JObject(List(("opt", JString("*")), ("left", convert(expr1)), ("right", convert(expr2))))
      }
  }
  val expr = Multiply(Number(1), Number(2))
  println("Expr result : " + ExprEvaluator.eval(expr))
  println("Json : " + JsonWriter.write(expr))

}