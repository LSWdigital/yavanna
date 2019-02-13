package yavanna

object spirvEnc {
  trait Line
  case class Op(op: Operation, parameters: List[ID]) extends Line
  case class Assign(id: ID, op: Operation, parameters: List[ID]) extends Line
  case class Comment(string: String) extends Line

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
  case object None extends ID
  case class Literal(s: String) extends ID {
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
}

import spirvEnc._
object spirvGen {

  //Type making stuff
  trait SSA
  trait Type
  case class makeType(v: ID, t: Type) extends SSA
  case class makeVar(v:ID, typeID: ID) extends SSA
  case class makeConst(v:ID, typeID:ID, l:ID) extends SSA
  case class makeFun(v: ID, typeID:ID, retType:ID, paramTypes: List[ID], params: List[ID], body: List[Line]) extends SSA

  //Types
  case object Float extends Type
  case object Void extends Type
  case class Vector(vt: ID, l: Int) extends Type
  case class Variable(vt: ID) extends Type
  case class Fun(pTypes: List[ID], retType: ID) extends Type

  class MissingParamError(s:String) extends Exception(s)

  // Go from making types and stuff to much simpler stuff.
  def lineGen(ir: SSA): List[Line] = ir match{
    case makeType(v, t) => t match {
      case Float => List(Assign(v, OpTypeFloat, List(Literal("32"))))
      case Void => List(Assign(v, OpTypeVoid, Nil))
      case Vector(vt, n) => List(Assign(v, OpTypeVector, List(vt, Literal(n.toString))))
      case Variable(vt) => List(Assign(v, OpTypePointer, List(Function, vt)))
      case Fun(paramTypes, retType) => List(Assign(v, OpTypeFunction, retType::paramTypes))
    }
  case makeVar(v, t) => List(Assign(v, OpVariable, List(t, Function)))
  case makeConst(v, t, l) => List(Assign(v, OpVariable, List(t, l)))
  case makeFun(v, t, retType, paramTypes, params, body) => Assign(v, OpFunction, List(retType, None, t))::
                                                           paramGen(paramTypes, params, body)
  }
  
  def paramGen(paramTypes: List[ID], params: List[ID], body: List[Line]): List[Line] = paramTypes match {
    case Nil => body :+ Op(OpFunctionEnd, Nil)
    case h::tail => params match {
      case Nil => throw new MissingParamError("Not enough params: SSA IR")
      case p::t => Assign(p, OpFunctionParameter, List(h)) :: 
                   paramGen(tail, t, body)
    }
  }
}

/*
import spirvGen._
object ssaGen {
  trait TypedSSA

  def extractTypes(ir: TypedSSA): (List[SSA], Map[Type, ID], List[SSA]) = ir match {
    
  }
}
*/
