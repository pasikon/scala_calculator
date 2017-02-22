package calculator

import scala.collection.immutable

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.foldLeft[Map[String, Signal[Double]]](Map()){
      (a: Map[String, Signal[Double]], t: (String, Signal[Expr])) => {
        val value: Signal[Double] = Var(eval(t._2(), namedExpressions))
        a + (t._1 -> value)
      }
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case l: Literal => l.v
      case r: Ref => eval(getReferenceExpr(r.name, references), references)
      case p: Plus => eval(p.a, references) + eval(p.b, references)
      case p: Minus => eval(p.a, references) - eval(p.b, references)
      case p: Times => eval(p.a, references) * eval(p.b, references)
      case p: Divide => eval(p.a, references) / eval(p.b, references)
      case Divide(_, Literal(0)) => Double.NaN
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
