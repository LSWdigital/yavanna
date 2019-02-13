package yavanna

object ast {
  trait Expression
    case class Var(name: String) extends Expression
    case class Declare(t: Type, v: Var) extends Expression
    case class I(int: Int) extends Expression
    case class F(float: Float) extends Expression
    case class B(bool: Boolean) extends Expression
    case class Assign(v: Var, exp: Expression) extends Expression
    case class New(t: Type, exp: Expression) extends Expression
    case class App(fun: Symbol, exp: Expression) extends Expression
    case class Swizzle(vec: Expression, order: Order) extends Expression

    case class Exps(vals: List[Expression]) extends Expression
    case class Vec(vals: List[Float]) extends Expression

    case class Symbol(sym: String)

  trait Order
    case object X extends Order
    case object Y extends Order
    case object Z extends Order
    case object W extends Order
    case object N extends Order
    case class P(pos1: Order, tail: Order) extends Order
    
    case class O(order: List[Order]) extends Order

  trait Type
    case class T(name: String) extends Type
    //class Fn(ts: Type, t: Type) extends Type

    //class Fun(ts: List[Type], t: T) extends Type

}

import ast._
import scala.util.parsing.combinator._
object parser extends JavaTokenParsers {
  // A program is a sequence of commands. This parser parses sequences of commands
  def program: Parser[Expression] =
    rep(command <~ ";")     ^^ { case cs=> Exps(cs) }

    // A command may or may not depend on the store. 
    // In any case, it's the fundamental unit of programming. This parser parses individual commands
    def command: Parser[Expression] =
      t ~ v ~ "=" ~ affix ^^ { case t~v~_~e=> Exps(List(Declare(t, v), Assign(v, e))) } |   //Declare & Assign
      t ~ v                 ^^ { case t~v => Declare(t, v) } |                          //Declare
      v ~ "=" ~ affix     ^^ { case v~_~e => Assign(v, e) } |                         //Assign
      affix               ^^ { case e => e } 
  
      def affix: Parser[Expression] =
        postfix ^^ {e => e} |
        expression ^^ {e => e} 

      
      //Infix expressions need to be parsed carefully
      def postfix: Parser[Expression] =
        expression ~ "." ~ rep(order)          ^^ { case e~_~o => Swizzle(e, O(o)) }        //swizzle

        // An expression evaluates to a value. This parser parses expressions
        def expression: Parser[Expression] =
          function ~ expression                   ^^ { case s~e => App(s, e)} |                             //application, (sqrt), min, max, clamp, dot, length
          // "false"                             ^^ { case _ => B(false) } |
          // "true"                              ^^ { case _ => B(true)  } |
          // wholeNumber                         ^^ { case x => I(x) } |
          "(" ~> floatingPointNumber ~ "," ~ floatingPointNumber ~ "," ~ floatingPointNumber ~ "," ~ floatingPointNumber <~ ")" ^^ {case x~_~y~_~z~_~w => Vec(List(x.toFloat, y.toFloat, z.toFloat, w.toFloat))} |
          //floatingPointNumber                 ^^ { case x => F(x.toFloat) } |
          v                                   ^^ { case s => s } |         //variable
          "(" ~> rep1sep(expression, ",") <~ ")"  ^^ { case e => Exps(e) }                                       //grouping
          
        // When we swizzle vectors, the order needs to be parsed. This parser does it for us, so we don't clog up the  expression parser.
        def order: Parser[Order] =
          "x"         ^^ {case _ => X } |
          "y"         ^^ {case _ => Y } |
          "z"         ^^ {case _ => Z } |
          "w"         ^^ {case _ => W }
        
        // Variables refer to some value stored somewhere.
        def v: Parser[Var]  =
          ident ^^ {case v => Var(v) }
        
        // Types have names that can be parsed.
        def t: Parser[Type] =
          "vec4" ^^ {case _ => T("vec4")}

        // Functions have names that can be parsed.
        def function: Parser[Symbol] =
          "dot" ^^ { case _ => Symbol("dot") } |
          "+"   ^^ { case _ => Symbol("+")   } |
          "*"   ^^ { case _ => Symbol("*")   } |
          "-"   ^^ { case _ => Symbol("-")   } |
          "/"   ^^ { case _ => Symbol("/")   }
}

