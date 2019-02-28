package yavanna
import ssaEnc._
object spirvEnc {
  def listString(l: List[ID]): String = l match {
    case Nil => ""
    case head::tail => " " + head.toString + listString(tail)
  }

  trait Line extends SSA
  case class Op(op: Operation, parameters: List[ID]) extends Line {
    override def toString = op.toString + listString(parameters)
  }
  case class Assign(id: ID, op: Operation, parameters: List[ID]) extends Line {
    override def toString = id.toString + " = " + op.toString + listString(parameters)
  }
  case class Comment(string: String) extends Line {
    override def toString = ";" + string
  }

  trait ID
  case class Id(string: String) extends ID {
    override def toString = "%" + string
  }
  //case class Id(int: Int) extends ID {
  //  override def toString = "%" + int.toString
  //}
  case object Output extends ID
  case object Input extends ID
  case object Function extends ID
  case object Non extends ID {
    override def toString = "None"
  }
  case class Lit(s: String) extends ID {
    override def toString = s
  }
  
  trait Operation
  //Types + Constants + Variables
  case object OpTypeVoid extends Operation
  case object OpTypeFunction extends Operation
  case object OpTypeFloat extends Operation
  case object OpTypePointer extends Operation
  case object OpTypeVector extends Operation
  
  case object OpVariable extends Operation
  case object OpConstant extends Operation
  case object OpFunction extends Operation
  case object OpFunctionParameter extends Operation

  //Operations
  case object OpLoad extends Operation
  case object OpStore extends Operation
  case object OpCompositeExtract extends Operation
  case object OpCompositeConstruct extends Operation
  case object OpReturn extends Operation
  case object OpReturnValue extends Operation
  case object OpFunctionEnd extends Operation
  case object OpFAdd extends Operation
  case object OpFSub extends Operation
  case object OpFMul extends Operation
  case object OpFDiv extends Operation
  case object OpLabel extends Operation

  def linesToString(ls: List[Line]): String = ls match {
    case Nil => ""
    case h::tail => h.toString + "\n" + linesToString(tail)
  }

  def genShader(ls: List[Line]): String = "; SPIR-V\n; Version: 1.0\n; Generator: Yavanna\n\n  OpCapability Shader\n  OpMemoryModel Logical Simple\n  OpEntryPoint Fragment %main \"main\" %outColor %fragColor\n  OpExecutionMode %main OriginUpperLeft\n\n  OpName %main \"main\"\n\n  OpName %outColor \"outColor\"\n  OpName %fragColor \"fragColor\"\n\n  OpDecorate %fragColor Location 0\n  OpDecorate %outColor Location 0\n"+ linesToString(ls)
}

import spirvEnc._
import ssaEnc._
import typedSSAEnc._
object spirvGen {

  //Type making stuff

  class MissingParamError(s:String) extends Exception(s)

  // Go from making types and stuff to much simpler stuff.
  def lineGen(ir: SSA): List[Line] = ir match{
    case makeType(v, t) => t match {
      case Float => List(Assign(v, OpTypeFloat, List(Lit("32"))))
      case Void => List(Assign(v, OpTypeVoid, Nil))
      case Vector(vt, n) => List(Assign(v, OpTypeVector, List(Id(vt.toString), Lit(n.toString))))
      case Variable(vt) => List(Assign(v, OpTypePointer, List(Function, Id(vt.toString))))
      case Fun(paramTypes, retType) => List(Assign(v, OpTypeFunction, retType::paramTypes))
    }
  case makeVar(v, t) => List(Assign(v, OpVariable, List(t, Function)))
  case makeConst(v, t, l) => List(Assign(v, OpConstant, List(t, l)))
  case makeFun(v, t, retType, paramTypes, params, body) => Assign(v, OpFunction, List(retType, Non, t))::paramGen(paramTypes, params, body) // Body : SSA -> Lines
  case other: Line => List(other)
  }
  
  def paramGen(paramTypes: List[ID], params: List[ID], body: List[SSA]): List[Line] = paramTypes match {
    case Nil => body.map(lineGen).flatten :+ Op(OpFunctionEnd, Nil)
    case h::tail => params match {
      case Nil => throw new MissingParamError("Not enough params: SSA IR")
      case p::t => Assign(p, OpFunctionParameter, List(h)) :: 
                   paramGen(tail, t, body.map(lineGen).flatten)
    }
  }
}

object ssaEnc {
  trait SSA
  trait Type
  case class makeType(v: ID, t: Type) extends SSA
  case class makeVar(v:ID, typeID: ID) extends SSA
  case class makeConst(v:ID, typeID:ID, l:ID) extends SSA
  case class makeFun(v: ID, typeID:ID, retType:ID, paramTypes: List[ID], params: List[ID], body: List[SSA]) extends SSA

  //Types
  case object Float extends Type
  case object Void extends Type
  case class Vector(vt: Type, l: Int) extends Type {
    override def toString = vt.toString + l.toString
  }
  case class Variable(vt: Type) extends Type {
    override def toString = vt.toString + "var"
  }
  case class Fun(pTypes: List[ID], retType: ID) extends Type {
    def functionString(strs: List[ID]): String = strs match {
      case Nil => ""
      case h::Nil => h.toString.slice(1, h.toString.length)
      case h::tail => h.toString.slice(1, h.toString.length) + "_" + functionString(tail)
    }
      override def toString = "fun" + functionString(pTypes :+ retType)
  }
  case class FunTRec(pTypes: List[Type], retType: Type) extends Type
}

import typedSSAEnc._
import ssaEnc._
import spirvEnc._
object ssaGen {


  def typesLookup(m: Map[Type, ID], ts:List[Type]): (Map[Type, ID], List[ID], List[SSA]) = ts match {
    case Nil => (m, Nil, Nil)
    case head::tail => typeLookup(m, head) match {
      case (typeMap, typeID, Some(ssa)) => typesLookup(typeMap, tail) match {
        case (finalMap, typeIDs, ssas) => (finalMap, typeID::typeIDs, ssa::ssas)
      }
      case (typeMap, typeID, None) => typesLookup(typeMap, tail) match {
        case (finalMap, typeIDs, ssas) => (finalMap, typeID::typeIDs, ssas)
      }
    }
  }

  def typeLookup(m: Map[Type, ID], t: Type): (Map[Type, ID], ID, Option[SSA]) = m get t match {
    case Some(id) => (m, id, None)
    case None => (m + (t -> Id(t.toString)), Id(t.toString), Some(makeType(Id(t.toString), t)))
  }

  def generateSSA(ir: List[TypedSSA], m: Map[Type, ID], typeInfo: List[SSA], body: List[SSA]): (List[SSA], Map[Type, ID], List[SSA]) = ir match {
    case Nil => (body, m, typeInfo)
    case head::tail => extractTypes(head, m) match {
      case (b, typeMap, t) => generateSSA(tail, typeMap, typeInfo ++ t, body :+ b) 
    }
  }

  def extractTypes(ir: TypedSSA, m: Map[Type, ID]): (SSA, Map[Type, ID], List[SSA]) = ir match {
    case Funct(v, retType, params, pTypes, body) => typeLookup(m, retType) match {
      case (typeMap, retTypeID, Some(retSSA)) => typesLookup(typeMap, pTypes) match {
        case (paramMap, paramTypeIDs, paramSSAs) => typeLookup(paramMap, Fun(paramTypeIDs, retTypeID)) match {
          case (funMap, funTypeID, Some(funSSA)) => generateSSA(body, funMap, retSSA::(paramSSAs :+ funSSA), Nil) match {
            case (bodySSA, finalMap, typeSSA) => (makeFun(v, funTypeID, retTypeID, paramTypeIDs, params, bodySSA), finalMap, typeSSA)
          }
          case (funMap, funTypeID, None) => generateSSA(body, funMap, retSSA::paramSSAs, Nil) match {
            case (bodySSA, finalMap, typeSSA) => (makeFun(v, funTypeID, retTypeID, paramTypeIDs, params, bodySSA), finalMap, typeSSA)
          }
        }
      }
      case (typeMap, retTypeID, None) => typesLookup(m, pTypes) match {
        case (paramMap, paramTypeIDs, paramSSAs) => typeLookup(paramMap, Fun(paramTypeIDs, retTypeID)) match {
          case (funMap, funTypeID, Some(funSSA)) => generateSSA(body, funMap, paramSSAs :+ funSSA, Nil) match { 
            case (bodySSA, finalMap, typeSSA) => (makeFun(v, funTypeID, retTypeID, paramTypeIDs, params, bodySSA), finalMap, typeSSA)
          }
          case (funMap, funTypeID, None) => generateSSA(body, funMap, paramSSAs, Nil) match {
            case (bodySSA, finalMap, typeSSA) => (makeFun(v, funTypeID, retTypeID, paramTypeIDs, params, bodySSA), finalMap, typeSSA)
          }
        }
      }
    } 
    
    case Var(v, t) => typeLookup(m, t) match {
      case (typeMap, typeID, Some(ssa)) => (makeVar(v, typeID), typeMap, List(ssa))
      case (typeMap, typeID, None) => (makeVar(v, typeID), typeMap, Nil)
    }
    case Const(v, t, l) => typeLookup(m, t) match {
      case (typeMap, typeID, Some(ssa)) => (Comment("Get global constant " + v.toString), typeMap, List(ssa, makeConst(v, typeID, l)))
      case (typeMap, typeID, None) => (Comment("Get global constant " + v.toString), typeMap, List(makeConst(v, typeID, l)))
    } 

    case Load(id, t, v) => typeLookup(m,t) match {
      case (typeMap, typeID, Some(ssa)) => (Assign(id, OpLoad, List(typeID, v)), typeMap, List(ssa))
      case (typeMap, typeID, None) => (Assign(id, OpLoad, List(typeID, v)), typeMap, Nil)
    }
    case Store(v, id) => (Op(OpStore, List(v, id)), m, Nil)                                     //OpStore
    case Extract(id, t, vec, index) => typeLookup(m,t) match {
      case (typeMap, typeID, Some(ssa)) => (Assign(id, OpCompositeExtract, List(typeID, vec, index)), typeMap, List(ssa))
      case (typeMap, typeID, None) => (Assign(id, OpCompositeExtract, List(typeID, vec, index)), typeMap, Nil)
    }

    case Construct(id, vt, vs) => typeLookup(m, vt) match {
      case (mapVec, vecType, Some(ssaVec)) => (Assign(id, OpCompositeConstruct, vecType::vs), mapVec, List(ssaVec))
      case (mapVec, vecType, None) => (Assign(id, OpCompositeConstruct, vecType::vs), mapVec, Nil)
    }

    case Label(id) => (Assign(id, OpLabel, Nil), m, Nil)
    case Return => (Op(OpReturn, Nil), m, Nil)
    case ReturnVal(id) => (Op(OpReturnValue, List(id)), m, Nil)

    case Arith(id, t, o, a, b) => typeLookup(m, t) match {
      case (typeMap, typeID, Some(ssa)) => (Assign(id, o, List(typeID, a, b)), typeMap, List(ssa))
      case (typeMap, typeID, None) => (Assign(id, o, List(typeID, a, b)), typeMap, Nil)
    }

    case Com(str) => (Comment(str), m, Nil)
  }
}

