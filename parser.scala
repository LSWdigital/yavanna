object ast {
  trait Expression
    case class Var(name: String) extends Expression
    case class Declare(t: Type, v: Var) extends Expression
    case class I(int: Int) extends Expression
    case class F(float: Float) extends Expression
    case class B(bool: Boolean) extends Expression
    class Pair(fst: Expression, snd: Expression) extends Expression
    class Assign(v: Var, exp: Expression) extends Expression
    class New(type: Type, exp: Expression) extends Expression
    class App(fun: Symbol, exp: Expression) extends Expression
    class Swizzle(fun: Symbol, order: Order) extends Expression

    class Exps(vals: List[Value]) extends Expression

  class Symbol(sym: String)

  trait Order
    case object X extends Order
    case object Y extends Order
    case object Z extends Order
    case object W extends Order
    case object N extends Order
    class P(pos1: Order, tail: Order) extends Order
    
    class O(order: List[Order]) extends Order

  trait Type
    case class T(name: String) extends Type
    class F(ts: Type, t: Type) extends Type

    class Fun(ts: List[Type], t: T) extends Type

}

import scala.util.parsing.combinator._
object parser extends JavaTokenParsers {
  def type: Parser[Type] =
    //type
  def expression: Parser[Expression] =
    expression ~ ";" ~ expression           ^^ { case e~es=> Pair(e, es) } |
    t ~ variable ~ "=" ~ expression         ^^ { case t~v~e=> Pair(Declare(t, v), Assign(v, e)) } |   //Declare & Assign
    t ~ variable                            ^^ { case t~v => Declare(t, v) } |                        //Declare
    v ~ "=" ~ expression                    ^^ { case v~e => Assign(v, e) } |                         //Assign
    "(" ~> expression <~ ")"                ^^ { case e => e } |                                      //grouping
    expression ~ "," ~ expression           ^^ { case e~es => Pair(e, es) } |                         //pair
    symbol ~ "(" ~> expression <~ ")"       ^^ { case s~e => App(s, e)} |                             //application, sqrt, min, max, clamp
    expression ~ "." ~ order                ^^ { case e~o => Swizzle(e, o) } |                        //swizzle
    expression ~ "||" ~ expression          ^^ { case e~es => App(Symbol("||"), Pair(e, es)) } |      //infix operators
    expression ~ "^^" ~ expression          ^^ { case e~es => App(Symbol("^^"), Pair(e, es)) } |      //infix operators
    expression ~ "&&" ~ expression          ^^ { case e~es => App(Symbol("&&"), Pair(e, es)) } |      //infix operators
    expression ~ "<" ~ expression           ^^ { case e~es => App(Symbol("<"), Pair(e, es)) } |       //infix operators
    expression ~ ">" ~ expression           ^^ { case e~es => App(Symbol(">"), Pair(e, es)) } |       //infix operators
    expression ~ "<=" ~ expression          ^^ { case e~es => App(Symbol("<="), Pair(e, es)) } |      //infix operators
    expression ~ ">=" ~ expression          ^^ { case e~es => App(Symbol(">="), Pair(e, es)) } |      //infix operators
    expression ~ "==" ~ expression          ^^ { case e~es => App(Symbol("=="), Pair(e, es)) } |      //infix operators
    expression ~ "!=" ~ expression          ^^ { case e~es => App(Symbol("!="), Pair(e, es)) } |      //infix operators
    expression ~ "-" ~ expression           ^^ { case e~es => App(Symbol("-"), Pair(e, es)) } |       //infix operators
    expression ~ "+" ~ expression           ^^ { case e~es => App(Symbol("+"), Pair(e, es)) } |       //infix operators
    expression ~ "/" ~ expression           ^^ { case e~es => App(Symbol("/"), Pair(e, es)) } |       //infix operators
    expression ~ "*" ~ expression           ^^ { case e~es => App(Symbol("*"), Pair(e, es)) } |       //infix operators
    "false"                                 ^^ { case _ => B(false) } |
    "true"                                  ^^ { case _ => B(true)  } |
    wholeNumber                             ^^ { case x => I(x) } |
    floatingPointNumber                     ^^ { case x => F(x) } |
    """.*"""                                      ^^ { case s => Var(s) } |           //variable
  def order: Parser[Order] =
    // position ~ order
    // nothing
  def symbol: Parser[Symbol] =
    // Function names, etc.
}
