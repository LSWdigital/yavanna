package yavanna

import ast._
object flattener {
  def flatten(exp: Expression): Expression = exp match {
      case Var(name) => Var(name)
      case Declare(t, v) => Declare(t, v)
      case I(x) => I(x)
      case F(f) => F(f)
      case B(b) => B(b)
      case Assign(v, e) => Assign(v, flatten(e)) //TODO: Closures
      case App(f, e) => Exps( List(flatten(Assign(Var("temp"), e)), 
                                   App(f, Var("temp")))) 
      case Swizzle(e, o) => Exps( 
                            List( 
                                flatten(Assign(Var("temp"), e)), 
                                Swizzle(Var("temp"), o)
                              ))
      case Vec(es) => Vec(es)
      case Exps(es) => Exps(ExpFlatten(Exps(es)))
  }

  def ExpFlatten(exp: Expression): List[Expression] = exp match {
    case Exps(es) => es match {
      case Nil => Nil
      case head :: tail => (head match{
        case Exps(i) => i.map(flatten _)
        case i => List(flatten(i))
      }) ::: ExpFlatten(Exps(tail))
    }
  }
}
