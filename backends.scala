package yavanna

import ast._
object flattener {
  def flatten(exp: Expression): Expression = exp match {
      case Var(name) => Var(name)
      case Declare(t, v) => Declare(t, v)
      case I(x) => I(x)
      case F(f) => F(f)
      case B(b) => B(b)
      case Assign(v, e) => Assign(v, flatten(e))
      case App(f, Exps(e)) => App(f, Exps(e map {exps => flatten(exps)}))
      case Swizzle(e, o) => Swizzle(flatten(e), o)
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

object continuationAST {
  trait Instruction
    case class CreateType(x: String, t: Type) extends Instruction
    case class Val(x: String, declaration: String) extends Instruction
    case class C_Extract(x: String, t: Type, v: String, i: Int) extends Instruction
    case class C_Construct(x: String, t: Type, v: List[String]) extends Instruction
    case class CallF(x: String, function: String, t: Type, v: List[String]) extends Instruction
    // +, -, *, /, .
}

/*import continuationAST._
object continuator {
  def continue(exp: Expression): (Int, List[Instruction], Map[String, String]) = 
      continue(exp, Nil, 1, Map.empty[String, String])

  def continue(exp: Expression, cont: List[Instruction], x: Int, 
               varTypes: Map[String, String]): 
              (Int, List[Instruction], Map[String, String]) = exp match {

    case Declare(T(t), Var(v)) => (x, cont, varTypes + (v -> t)) //Need to create types
    case Assign(Var(v), e) => continue(e, cont, x, varTypes) match {
      case (i, instr::c, m) => (i, Val(v, instrVal(instr))::c, m)
    }
    case App(f, Exps(e)) => listContinuation(e, cont, x, varTypes) match {
      case (i, c, m, l) => (i + 1, CallF(i.toString, getFun(f), getType(f), l)::c, m)
    }
    case Exps(e) => listContinuation match {
      case (i, c, m, l) => (i, c, m)
    }
  }

  def listContinuation(exps: List[Expression], cont: List[Instruction], 
                       x: Int, varTypes: Map[String, String]): 
                      (Int, List[Instruction], Map[String, String], List[Int]) =
                        exps match {
    case Nil => Nil
    case e::es => continue(e, cont, x, varTypes) match {
      case (i, c, m) => listContinuation(es, c, i, m) match {
        case (j, k, n, l) => (j, k, n, i::l)
      }
    } 
  }

  def getFun()

  def instrVal(instr: Instruction): String = instr match {
    case CreateType(x, _) => x
    case Val(x, _) => x
    case C_Extract(x, _, _ ,_) => x
    case C_Construct(x, _, _) => x
    case CallF(x, _, _) => x
  }
}
*/
