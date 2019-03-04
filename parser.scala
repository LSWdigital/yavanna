package yavanna

object ast {
  trait Expression
  case class ETail(name: String, retType: String, params: List[TypeNamePair], b: Expression, base: Expression, recParams: List[Expression]) extends Expression

  case class EAssign(name: String, value: Expression) extends Expression
  case class EName(name: String) extends Expression
  case class EFun(name: String, retType:String, params: List[TypeNamePair], body: List[Expression]) extends Expression
  case class EVar(name: String, t: String) extends Expression
  case class EConst(name: String, t:String, l: String) extends Expression
  case class ELoad(name: String) extends Expression
  case class EStore(name: String, value: Expression) extends Expression
  case class EExtract(vec: Expression, index: Int) extends Expression
  case class EConstruct(vs: List[Expression]) extends Expression
  case object EReturn extends Expression 
  case class EReturnVal(v: Expression) extends Expression
  case class EComment(comment: String) extends Expression
  case class EMul(a: Expression, b: Expression) extends Expression
  case class EDiv(a: Expression, b: Expression) extends Expression
  case class ESub(a: Expression, b: Expression) extends Expression
  case class EAdd(a: Expression, b: Expression) extends Expression
  case class ESqrt(vec: Expression) extends Expression
  case class ELT(a: Expression, b: Expression) extends Expression
  case class EGT(a: Expression, b: Expression) extends Expression

  case class TypeNamePair(name: String, t: String)
}

import ast._
import scala.util.parsing.combinator._
object parser extends JavaTokenParsers {
  // A program is a sequence of commands. This parser parses sequences of commands
  def program: Parser[List[Expression]] =
    repsep(command, ";")     ^^ { case cs=> cs }

  def command: Parser[Expression] = 
    ident ~ "=" ~ command                                            ^^ { case v~_~e => EAssign(v, e) }|
    "fun " ~ ident ~ "(" ~ repsep(names, ",") ~ ")" ~ ident ~ block  ^^ { case _~name~_~params~_~t~body => EFun(name, t, params, body) }|
    "var " ~ ident ~ "|" ~ ident                                     ^^ { case _~name~_~t => EVar(name, t)}|
    "const " ~ ident ~ floatingPointNumber                           ^^ { case _~name~v => EConst(name, "Float", v)}|
    "!" ~ ident                                                      ^^ { case _~name => ELoad(name) }|
    ident ~ ":=" ~ command                                           ^^ { case name~_~v => EStore(name, v)}|
    "get " ~ command ~ "." ~ "X"                                     ^^ { case _~name~_~_ => EExtract(name, 0)}|
    "get " ~ command ~ "." ~ "Y"                                     ^^ { case _~name~_~_ => EExtract(name, 1)}|
    "get " ~ command ~ "." ~ "Z"                                     ^^ { case _~name~_~_ => EExtract(name, 2)}|
    "get " ~ command ~ "." ~ "W"                                     ^^ { case _~name~_~_ => EExtract(name, 3)}|
    "Vec2(" ~> command ~ "," ~ command <~ ")"                        ^^ { case x~_~y => EConstruct(List(x, y))}|
    "Vec3(" ~> command ~ "," ~ command ~ "," ~ command <~ ")"        ^^ { case x~_~y~_~z => EConstruct(List(x, y, z))}|
    "Vec4(" ~> command ~ "," ~ command ~ "," ~ command ~ "," ~ command <~ ")" ^^ { case x~_~y~_~z~_~w => EConstruct(List(x, y, z, w))}|
    "return"                                                         ^^ { case _ => EReturn}| //Not needed?
    "return(" ~> command <~ ")"                                      ^^ { case x => EReturnVal(x)}| //Also,not Needed?
    "*(" ~> command ~ "," ~ command <~ ")"                           ^^ { case a~_~b => EMul(a, b)}|
    "/(" ~> command ~ "," ~ command <~ ")"                           ^^ { case a~_~b => EDiv(a, b)}|
    "-(" ~> command ~ "," ~ command <~ ")"                           ^^ { case a~_~b => ESub(a, b)}|
    "+(" ~> command ~ "," ~ command <~ ")"                           ^^ { case a~_~b => EAdd(a, b)}|
    "sqrt(" ~> command <~ ")"                                         ^^ { case vec => ESqrt(vec)}|
    "lt(" ~> command ~ "," ~ command <~ ")"                          ^^ { case a~_~b => ELT(a, b)}|
    "gt(" ~> command ~ "," ~ command <~ ")"                          ^^ { case a~_~b => EGT(a, b)}|
    "fun " ~ ident ~ "(" ~ repsep(names, ",") ~ ")" ~ ident ~ "when" ~ command ~ "is" ~ command ~ "else" ~ ident ~ "(" ~ repsep(command, ",") ~ ")" ^^ { case _~name~_~params~_~retType~_~b~_~base~_~x~_~recParams~_ => if((name == x)&(params.length == recParams.length)){ETail(name, retType, params, b, base, recParams)}else{EComment("Fail")}}|
    ident                                                            ^^ { case name => EName(name) }

  def block: Parser[List[Expression]] =
    "{" ~> program <~ "}" ^^ {case p => p}

  def names: Parser[TypeNamePair] =
    ident ~ "|" ~ ident ^^ {case name~_~t => TypeNamePair(name, t)}
}

import typedASTEnc._
import strippedASTEnc._
import ssaEnc._
object typeGen {
  def expressionsToTypedAST(es:List[Expression], m: Map[String, Type]): (List[TypedAST], Map[String, Type]) = es match {
    case Nil => (Nil, m)
    case head::tail => expressionToTypedAST(head, m) match{
      case (h, types) => expressionsToTypedAST(tail, types) match{
        case (t, ts) => (h::t, ts)
      }
    }
  }

  def splitParams(ps: List[TypeNamePair], m: Map[String, Type]): (List[String], List[Type], Map[String, Type]) = ps match {
    case Nil => (Nil, Nil, m)
    case head::tail => head match{
      case TypeNamePair(name, t) => assignType(name, m, t) match{
        case (pType, types) => splitParams(tail, types) match{
          case (ns, ts, finalTypes) => (name::ns, pType::ts, finalTypes)
        }
      }
    }
  }
  
  def getType(v:String, m: Map[String, Type]): Type = m(v) 

  def assignType(n: String, m: Map[String, Type], t:String): (Type, Map[String, Type]) = if (m contains n) {
    (m(n), m)
  } else {
    t match {
      case "Float" => (Float, m + (n -> Float))
      case "Bool" => (Bool, m + (n -> Bool))
      case "Unit" => (Void, m + (n -> Void))
      case "Vec2" => (Vector(Float, 2), m + (n -> Vector(Float, 2)))
      case "Vec3" => (Vector(Float, 2), m + (n -> Vector(Float, 2)))
      case "Vec4" => (Vector(Float, 2), m + (n -> Vector(Float, 2)))
    }
  }

  def storeAll(ps: List[String], recParams: List[TypedAST]) : List[TypedAST] = ps match {
    case Nil => Nil
    case h::t => recParams match {
      case head::tail => TStore(h + "_var", head)::storeAll(t, tail)
    }
  }

  def buildLoop(name: String, retType: Type, ps: List[String], ts: List[Type], b: TypedAST, base: TypedAST, recParams: List[TypedAST], m: Map[String, Type]): TypedAST = 
    TFun(name, retType, ts.map(x => x + "_param"), ts, 
      ps.map(x => TVar(x + "_var", m(x))) ++
      ps.map(x => TStore(x + "_var", TName(x + "_param", m(x)))) :+
      TLoop(ps.map(x => TAssign(x, TLoad(m(x), x  + "_var"))) :+b, Nil, storeAll(ps, recParams), List(TReturnVal(base)))
      )

  def expressionToTypedAST(e: Expression, m: Map[String, Type]): (TypedAST, Map[String, Type]) = e match {
  
      case ETail(name, retType, params, b, base, recParams) => splitParams(params, m) match {
        case (ps, ts, typeMap) => expressionToTypedAST(b, typeMap) match {
          case (bT, bM) => expressionToTypedAST(base, bM) match {
            case (baseT, baseMap) => expressionsToTypedAST(recParams, baseMap) match {
              case (paramsT, paramMap) => assignType(name, paramMap, retType) match {
                case (retT, finalTypes) => (buildLoop(name, retT, ps, ts, bT, baseT, paramsT, finalTypes), finalTypes)
              }
            }
          }
        }
      }

      case EAssign(name, value) => expressionToTypedAST(value, m) match {
        case (v, types) => (TAssign(name, v), types)
      }
      case EName(name) => (TName(name, getType(name, m)), m)
      case EFun(name, t, params, body) => splitParams(params, m) match {
        case (ps, ts, typeMap) => assignType(name, typeMap, t) match {
          case (retType, types) => expressionsToTypedAST(body, types) match{
            case (fBody, finalTypes) => (TFun(name, retType, ps, ts, fBody), finalTypes)
          }
        }
      }
      case EVar(name, t) => assignType(name, m, t) match {
        case (vType, types) => (TVar(name, vType), types)
      }
      case EConst(name, t, l) => assignType(name, m, t) match {
        case (cType, types) => (TConst(name, cType, FloatLiteral(l.toFloat)), types)
      }
      case ELoad(name) => (TLoad(getType(name, m), name), m)
      case EStore(name, value) => expressionToTypedAST(value, m) match {
        case (v, types) => (TStore(name,v), types)
      }
      case EExtract(vec, index) => expressionToTypedAST(vec, m) match{
        case (v, types) => (TExtract(v, IntLiteral(index)), types)
      }
      case EConstruct(els) => expressionsToTypedAST(els, m) match {
        case (es, types) => (TConstruct(es), types)
      }
      case EReturn => (TReturn, m)
      case EReturnVal(v) => expressionToTypedAST(v, m) match {
        case (value, types) => (TReturnVal(value), types)
      }
      case EMul(a, b) => expressionToTypedAST(a, m) match {
        case(aAST, aTypes) => expressionToTypedAST(b, aTypes) match {
          case(bAST, types) => (TMul(aAST, bAST), types)
        }
      }
      case EDiv(a, b) => expressionToTypedAST(a, m) match {
        case(aAST, aTypes) => expressionToTypedAST(b, aTypes) match {
          case(bAST, types) => (TDiv(aAST, bAST), types)
        }
      }
      case ESub(a, b) => expressionToTypedAST(a, m) match {
        case(aAST, aTypes) => expressionToTypedAST(b, aTypes) match {
          case(bAST, types) => (TSub(aAST, bAST), types)
        }
      }
      case EAdd(a, b) => expressionToTypedAST(a, m) match {
        case(aAST, aTypes) => expressionToTypedAST(b, aTypes) match {
          case(bAST, types) => (TAdd(aAST, bAST), types)
        }
      }
      case ELT(a, b) => expressionToTypedAST(a, m) match {
        case(aAST, aTypes) => expressionToTypedAST(b, aTypes) match {
          case(bAST, types) => (TLT(aAST, bAST), types)
        }
      }
      case EGT(a, b) => expressionToTypedAST(a, m) match {
        case(aAST, aTypes) => expressionToTypedAST(b, aTypes) match {
          case(bAST, types) => (TGT(aAST, bAST), types)
        }
      }
      case ESqrt(a) => expressionToTypedAST(a, m) match {
        case (v, types) => (TSqrt(v), types)
      }
  }
}
