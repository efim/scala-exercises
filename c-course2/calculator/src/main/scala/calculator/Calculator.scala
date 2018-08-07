package calculator

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

    namedExpressions map { case (name, expr) =>
      (name, Signal {eval(Set(name), expr(), namedExpressions)} )
      }

  }

  def eval(parentNames: Set[String], expr: Expr, refs: Map[String, Signal[Expr]]): Double = {

    val cleaned = removeCircular(parentNames, expr)

    def evalParts(a: Expr, b: Expr, op: (Double, Double) => Double) =
      op(eval(parentNames, a, refs), eval(parentNames, b, refs))

    try {
      cleaned match {
        case Literal(value) => value
        case Ref(name) => eval(parentNames + name, getReferenceExpr(name, refs), refs)
        case Plus(a, b) => evalParts(a,b,_+_)
        case Minus(a, b) => evalParts(a,b,_-_)
        case Times(a, b) => evalParts(a,b,_*_)
        case Divide(a, b) => evalParts(a,b,_/_)
      }
    } catch {
      case _: Exception => Double.NaN
    }
  }

  private def removeCircular(parentNames: Set[String], expr: Expr): Expr = {
    if (!hasDependency(parentNames, expr)) expr
    else Literal(Double.NaN)
  }

  private def hasDependency(parentNames: Set[String], expr: Expr): Boolean = {
    def haveDependencies(a: Expr, b: Expr) =
      hasDependency(parentNames, a) || hasDependency(parentNames, b)

    expr match {
      case Ref(name) => parentNames.contains(name)
      case Plus(a, b) => haveDependencies(a, b)
      case Minus(a, b) => haveDependencies(a, b)
      case Times(a, b) => haveDependencies(a, b)
      case Divide(a, b) => haveDependencies(a, b)
      case _ => false
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
