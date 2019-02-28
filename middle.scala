package yavanna

import spirvEnc._
import ssaEnc._
object typedSSAEnc {
  trait TypedSSA
  case class Funct(v:ID, retType:Type, params: List[ID], pTypes:List[Type], body: List[TypedSSA]) extends TypedSSA
  case class Var(v:ID, t:Type) extends TypedSSA
  case class Const(v:ID, t:Type, l:ID) extends TypedSSA

  case class Load(id:ID, t:Type, v:ID) extends TypedSSA                 //OpLoad
  case class Store(v:ID, id:ID) extends TypedSSA                         //OpStore
  case class Extract(id:ID, t:Type, vec: ID, ind:ID) extends TypedSSA   //OpCompositeExtract
  case class Construct(id:ID, vt:Type, vs: List[ID]) extends TypedSSA    //OpCompositeConstruct
  
  case class Label(id:ID) extends TypedSSA
  case object Return extends TypedSSA
  case class ReturnVal(id:ID) extends TypedSSA                          //OpReturnValue
  
  case class Com(str: String) extends TypedSSA

  case class Arith(id:ID, t:Type, op:Operation, a:ID, b:ID) extends TypedSSA           //OpFAdd

}

object strippedASTEnc{
  trait Literal
  case class FloatLiteral(f: Float) extends Literal {
    override def toString = f.toString
  }
  case class IntLiteral(i: Integer) extends Literal {
    override def toString = i.toString
  }
  
  trait strippedAST
  case class ASTAssign(name: String, value: strippedAST) extends strippedAST
  case class ASTName(name: String) extends strippedAST

  case class ASTFun(name: String, retType:Type, params:List[String], pTypes:List[Type], body: List[strippedAST]) extends strippedAST

  case class ASTVar(name:String, t:Type) extends strippedAST
  case class ASTConst(name:String, t:Type, l: Literal) extends strippedAST
  
  case class ASTLoad(t:Type, v:String) extends strippedAST                 //OpLoad
  case class ASTStore(v:String, id:strippedAST) extends strippedAST                       //OpStore
  
  case class ASTExtract(t:Type, vec: strippedAST, l: Literal) extends strippedAST
  case class ASTConstruct(vt:Type, vs: List[strippedAST]) extends strippedAST

  case class ASTLabel(name: String) extends strippedAST
  case object ASTReturn extends strippedAST
  case class ASTReturnVal(v: strippedAST) extends strippedAST
  
  case class ASTComment(str: String) extends strippedAST
  case class ASTArith(t:Type, op:Operation, a: strippedAST, b: strippedAST) extends strippedAST
}

import spirvEnc._
import typedSSAEnc._
import strippedASTEnc._
object typedSSAGen{
  trait IDTree
  case class Branch(list: List[IDTree], t: ID) extends IDTree
  case class Leaf(t: ID) extends IDTree

  def top(tree: IDTree): ID = tree match{
    case Branch(_, id) => id
    case Leaf(id) => id
  } 

  def genIDTrees(trees: List[strippedAST], intID: Integer): (List[IDTree], Integer) = trees match {
    case Nil => (Nil, intID)
    case head::tail => genIDTree(head, intID) match {
      case (tree, i) => genIDTrees(tail, i) match {
        case (idTrees, finalID) => (tree::idTrees, finalID)
      }
    }
  }

  def genIDTree(tree: strippedAST, intID: Integer): (IDTree, Integer) = tree match{
    case ASTName(name) => (Leaf(Id(name)), intID)
    case ASTAssign(name, value) => genIDTree(value, intID) match {
      case (tree, i) => (Branch(List(tree), Id(name)), i)
    }
    case ASTFun(name, retType, params, pTypes, body) => genIDTrees(body, intID) match {
      case (trees, i) => (Branch(trees, Id(name)), i)
    }

    case ASTVar(name, t) => (Leaf(Id(name)), intID)
    case ASTConst(name, t, l) => (Leaf(Id(name)), intID)
    case ASTLabel(name) => (Leaf(Id(name)), intID)
    case ASTReturn => (Leaf(Id("Nil")), intID)
    case ASTReturnVal(v) => genIDTree(v, intID) match {
        case (tree, i) => (Branch(List(tree), top(tree)), i)
    }

    case ASTComment(comment) => (Leaf(Id("Nil")), intID)

    case ASTExtract(t, vec, l) => genIDTree(vec, intID) match {
      case (tree, i) => (Branch(List(tree), Id((i+1).toString)), i+1)
    }
    case ASTConstruct(vt, vs) => genIDTrees(vs, intID) match {
      case (trees, i) => (Branch(trees, Id((i+1).toString)), i+1)
    }
    case ASTArith(t, op, a, b) => genIDTree(a, intID) match {
      case(atree, aID) => genIDTree(b, aID) match {
        case(btree, i) => (Branch(List(atree, btree), Id((i+1).toString)), i+1)
      }
    }
    case ASTStore(n, v) => genIDTree(v, intID) match {
      case(tree, i) => (Branch(List(tree), Id(n)), i)
    }
    case ASTLoad(t, v) => (Leaf(Id((intID+1).toString)), intID+1)
  }

  def genSSATrees(asts: List[strippedAST], idTrees: List[IDTree]): List[TypedSSA] = asts match {
    case Nil => Nil
    case head::tail => idTrees match{
      case idHead::idTail => genSSATree(head, idHead) ++ genSSATrees(tail, idTail)
    }
  }

  def genSSATree(ast: strippedAST, idTree: IDTree): List[TypedSSA] = ast match {
    case ASTName(name) => Nil
    case ASTAssign(name, value) => idTree match {
      case Branch(List(tree), _) => tree match {
        case Branch(trees, id) => genSSATree(value, Branch(trees, Id(name)))
        case Leaf(id) => genSSATree(value, Leaf(Id(name)))
      }
    }
    case ASTFun(name, retType, params, pTypes, body) => idTree match{
      case Branch(trees, id) => List(Funct(id, retType, params.map((s:String) => Id(s)), pTypes, genSSATrees(body, trees)))
    }

    case ASTVar(name, t) => List(Var(Id(name), t))
    case ASTConst(name, t, l) => List(Const(Id(name), t, Lit(l.toString)))
    case ASTLabel(name) => List(Label(Id(name)))
    case ASTReturn => List(Return)
    case ASTReturnVal(v) => idTree match {
      case Branch(List(tree), id) => genSSATree(v, tree) :+ ReturnVal(top(tree))
    }

    case ASTComment(comment) => List(Com(comment))
    
    case ASTExtract(t, vec, l) => idTree match {
      case Branch(List(tree), id) => genSSATree(vec, tree) :+ Extract(id, t, top(tree), Lit(l.toString))
    }
    case ASTConstruct(vt, vs) => idTree match {
      case Branch(trees, id) => genSSATrees(vs, trees) :+ Construct(id, vt, trees.map(top))
    }
    case ASTArith(t, op, a, b) => idTree match {
      case Branch(List(aTree, bTree), id) => genSSATrees(List(a, b), List(aTree, bTree)) :+ Arith(id, t, op, top(aTree), top(bTree))
    }

    case ASTStore(n, v) => idTree match {
      case Branch(List(tree), id) => genSSATree(v, tree) :+ Store(id, top(tree))
    }
    case ASTLoad(t, v) => idTree match {
      case Leaf(id) => List(Load(id, t, Id(v)))
    }
  }
}

object typedASTEnc {
  trait TypedAST
  case class TAssign(name: String, value:TypedAST) extends TypedAST
  case class TName(name: String, t: Type) extends TypedAST
  case class TFun(name: String, retType: Type, params:List[String], pTypes:List[Type], body:List[TypedAST]) extends TypedAST
  case class TVar(name:String, t:Type) extends TypedAST
  case class TConst(name:String, t:Type, l:Literal) extends TypedAST
  case class TLoad(t: Type, v: String) extends TypedAST
  case class TStore(n: String, v: TypedAST) extends TypedAST
  case class TExtract(vec: TypedAST, l:Literal) extends TypedAST
  case class TConstruct(vs: List[TypedAST]) extends TypedAST
  case class TLabel(name: String) extends TypedAST
  case object TReturn extends TypedAST
  case class TReturnVal(v: TypedAST) extends TypedAST
  case class TComment(comment: String) extends TypedAST
  case class TMul(a: TypedAST, b:TypedAST) extends TypedAST
  case class TDiv(a: TypedAST, b:TypedAST) extends TypedAST
  case class TAdd(a: TypedAST, b:TypedAST) extends TypedAST
  case class TSub(a: TypedAST, b:TypedAST) extends TypedAST
}

import typedASTEnc._

object strippedASTGen {
  def treeTypes(trees: List[TypedAST]): Type = trees match {
    case Nil => Void
    case head::tail => treeTypes(tail, treeType(head))
  }
  def treeTypes(trees: List[TypedAST], t: Type): Type = trees match {
    case Nil => t
    case head::tail => if(treeType(head) == t) treeTypes(tail, t) else throw new Exception("Mismatched Types!")
  }

  def treeType(tree: TypedAST): Type = tree match {
    case TAssign(_, _) => Void 
    case TName(_, t) => t
    case TFun(_, retType, _, pTypes, _) => FunTRec(pTypes, retType)
    case TVar(_, t) => t
    case TConst(_, t, _) => t
    case TLoad(t, _) => t
    case TStore(_, _) => Void
    case TExtract(vec, _) => treeType(vec) match {
      case Vector(vt, _) => vt
    }
    case TConstruct(vs) => Vector(treeTypes(vs), vs.length)
    case TLabel(_) => Void
    case TReturn => Void
    case TReturnVal(_) => Void
    case TComment(_) => Void
    case TMul(a, b) => treeTypes(List(a, b))
    case TDiv(a, b) => treeTypes(List(a, b))
    case TAdd(a, b) => treeTypes(List(a, b))
    case TSub(a, b) => treeTypes(List(a, b))
  }
  
  def generateSAST(tree: TypedAST): strippedAST = tree match {
    case TAssign(n, v) => ASTAssign(n, generateSAST(v))
    case TName(n, t) => ASTName(n)
    case TFun(name, retType, params, pTypes, body) => ASTFun(name, retType, params, pTypes, body.map(generateSAST))
    case TVar(n, t) => ASTVar(n, t)
    case TConst(n, t, l) => ASTConst(n, t, l)
    case TLoad(t, n) => ASTLoad(t, n)
    case TStore(v, ast) => ASTStore(v, generateSAST(ast))
    case TExtract(vec, l) => ASTExtract(treeType(TExtract(vec, l)), generateSAST(vec), l)
    case TConstruct(vs) => ASTConstruct(treeType(TConstruct(vs)),  vs.map(generateSAST)) 
    case TLabel(n) => ASTLabel(n)
    case TReturn => ASTReturn
    case TReturnVal(v) => ASTReturnVal(generateSAST(v))
    case TComment(s) => ASTComment(s)
    case TMul(a, b) => ASTArith(treeTypes(List(a, b)), OpFMul, generateSAST(a), generateSAST(b))
    case TDiv(a, b) => ASTArith(treeTypes(List(a, b)), OpFDiv, generateSAST(a), generateSAST(b))
    case TAdd(a, b) => ASTArith(treeTypes(List(a, b)), OpFAdd, generateSAST(a), generateSAST(b))
    case TSub(a, b) => ASTArith(treeTypes(List(a, b)), OpFSub, generateSAST(a), generateSAST(b))
  }
}
