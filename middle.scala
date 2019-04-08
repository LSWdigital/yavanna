package yavanna

import spirvEnc._
import ssaEnc._
object typedSSAEnc {
  trait TypedSSA
  case class Funct(v:ID, retType:Type, params: List[ID], pTypes:List[Type], body: List[TypedSSA]) extends TypedSSA
  case class Call(v:ID, retType:Type, name:ID, ps: List[ID]) extends TypedSSA

  case class Var(v:ID, t:Type) extends TypedSSA
  case class Const(v:ID, t:Type, l:ID) extends TypedSSA
  case class Null(id:ID, t:Type) extends TypedSSA

  case class Load(id:ID, t:Type, v:ID) extends TypedSSA                 //OpLoad
  case class Store(v:ID, id:ID) extends TypedSSA                         //OpStore
  case class Extract(id:ID, t:Type, vec: ID, ind:ID) extends TypedSSA   //OpCompositeExtract
  case class Construct(id:ID, vt:Type, vs: List[ID]) extends TypedSSA    //OpCompositeConstruct
  
  case class Label(id:ID) extends TypedSSA
  case object Return extends TypedSSA
  case class ReturnVal(id:ID) extends TypedSSA                          //OpReturnValue
  
  case class Com(str: String) extends TypedSSA

  case class Arith(id:ID, t:Type, op:Operation, a:ID, b:ID) extends TypedSSA           //OpFAdd
  case class SquareRoot(id:ID, t:Type, x:ID) extends TypedSSA
  case class VectorLength(id:ID, t:Type, x:ID) extends TypedSSA
  case class LT(id: ID, a: ID, b: ID) extends TypedSSA
  case class GT(id: ID, a: ID, b: ID) extends TypedSSA

  case class BranchUC(id: ID) extends TypedSSA
  case class BranchConditional(bool: ID, thn: ID, els: ID) extends TypedSSA
  case class LoopMerge(mergeID: ID, contID: ID) extends TypedSSA
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
  case class ASTCall(retType: Type, name: String, params: List[strippedAST]) extends strippedAST

  case class ASTVar(name:String, t:Type) extends strippedAST
  case class ASTConst(name:String, t:Type, l: Literal) extends strippedAST
  case class ASTNull(t:Type) extends strippedAST
  
  case class ASTLoad(t:Type, v:String) extends strippedAST                 //OpLoad
  case class ASTStore(v:String, id:strippedAST) extends strippedAST                       //OpStore
  
  case class ASTExtract(t:Type, vec: strippedAST, l: Literal) extends strippedAST
  case class ASTConstruct(vt:Type, vs: List[strippedAST]) extends strippedAST

  case class ASTLabel(name: String) extends strippedAST
  case object ASTLabelAnon extends strippedAST
  case object ASTReturn extends strippedAST
  case class ASTReturnVal(v: strippedAST) extends strippedAST
  case class ASTLoop(cond: List[strippedAST], compute: List[strippedAST], cont: List[strippedAST], merge: List[strippedAST]) extends strippedAST
  
  case class ASTComment(str: String) extends strippedAST
  case class ASTArith(t:Type, op:Operation, a: strippedAST, b: strippedAST) extends strippedAST
  case class ASTSqrt(t:Type, x: strippedAST) extends strippedAST
  case class ASTLength(t:Type, x: strippedAST) extends strippedAST
  case class ASTLt(a:strippedAST, b:strippedAST) extends strippedAST
  case class ASTGt(a:strippedAST, b:strippedAST) extends strippedAST
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
    case ASTCall(retType, name, params) => genIDTrees(params, intID) match {
      case (trees, i) => (Branch(trees, Id((i+1).toString)), i+1)
    }

    case ASTVar(name, t) => (Leaf(Id(name)), intID)
    case ASTConst(name, t, l) => (Leaf(Id(name)), intID)
    case ASTNull(t) => (Leaf(Id(t.toString + "_null")), intID)
    case ASTLabel(name) => (Leaf(Id(name)), intID)
    case ASTLabelAnon => (Leaf(Id((intID + 1).toString)), intID + 1)
    case ASTReturn => (Leaf(Id("Nil")), intID)
    case ASTReturnVal(v) => genIDTree(v, intID) match {
        case (tree, i) => (Branch(List(tree), top(tree)), i)
    }
    
    case ASTLoop(cond, comp, cont, merge) => genIDTrees(cond, intID + 1) match { //header(intID)
      case (condTrees, i) => genIDTrees(comp, i+1) match { //cond(i)
        case (compTrees, j) => genIDTrees(cont, j+1) match { //comp(j)
          case (contTrees, k) => genIDTrees(merge, k+1) match {
            case (mergeTrees, l) => (Branch( List( Branch(condTrees, Id((i+1).toString)), 
                                                   Branch(compTrees, Id((j+1).toString)), 
                                                   Branch(contTrees, Id((k+1).toString)), 
                                                   Branch(mergeTrees, Id((l+1).toString))), 
                                             Id((intID+1).toString)), l+1)
          }
        } 
      }
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

    case ASTLt(a: strippedAST, b: strippedAST) => genIDTree(a, intID) match {
      case(aTree, aID) => genIDTree(b, aID) match {
        case(bTree, i) => (Branch(List(aTree, bTree), Id((i+1).toString)), i+1)
      }
    }

    case ASTGt(a: strippedAST, b: strippedAST) => genIDTree(a, intID) match {
      case(aTree, aID) => genIDTree(b, aID) match {
        case(bTree, i) => (Branch(List(aTree, bTree), Id((i+1).toString)), i+1)
      }
    }

    case ASTSqrt(t, x) => genIDTree(x, intID) match {
      case(tree, i) => (Branch(List(tree), Id((i+1).toString)), i+1)
    }

    case ASTLength(t, x) => genIDTree(x, intID) match {
      case(tree, i) => (Branch(List(tree), Id((i+1).toString)), i+1) 
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
    case ASTCall(retType, name, params) => idTree match{
      case Branch(trees, id) => genSSATrees(params, trees):+ Call(id, retType, Id(name), trees.map(top)) 
    }

    case ASTVar(name, t) => List(Var(Id(name), t))
    case ASTConst(name, t, l) => List(Const(Id(name), t, Lit(l.toString)))
    case ASTNull(t) => List(Null(Id(t.toString + "_null"), t))
    case ASTLabel(name) => List(Label(Id(name)))
    case ASTLabelAnon => idTree match {
      case Leaf(id) => List(Label(id))
    }
    case ASTReturn => List(Return)
    case ASTReturnVal(v) => idTree match {
      case Branch(List(tree), id) => genSSATree(v, tree) :+ ReturnVal(top(tree))
    }
    case ASTLoop(cond, comp, cont, merge) => idTree match {
      case Branch(List(Branch(condTrees, condID), Branch(compTrees, compID), Branch(contTrees, contID), Branch(mergeTrees, mergeID)), headerID) => List(
        BranchUC(headerID),
        Label(headerID),
        LoopMerge(mergeID, contID),
        BranchUC(condID),
        Label(condID)) ++ 
        genSSATrees(cond, condTrees) ++  
        List(BranchConditional(top(condTrees.last), compID, mergeID), //Branch
        Label(compID)) ++
        genSSATrees(comp, compTrees) ++
        List(BranchUC(contID),
        Label(contID)) ++
        genSSATrees(cont, contTrees) ++
        List(BranchUC(headerID),
        Label(mergeID)) ++
        genSSATrees(merge, mergeTrees)
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

    case ASTSqrt(t, x) => idTree match {
      case Branch(List(tree), id) => genSSATree(x, tree) :+ SquareRoot(id, t, top(tree))
    }

    case ASTLength(t, x) => idTree match {
      case Branch(List(tree), id) => genSSATree(x, tree) :+ VectorLength(id, t, top(tree))
    }

    case ASTLt(a, b) => idTree match {
      case Branch(List(aTree, bTree), id) => genSSATrees(List(a, b), List(aTree, bTree)) :+ LT(id, top(aTree), top(bTree))
    }

    case ASTGt(a, b) => idTree match {
      case Branch(List(aTree, bTree), id) => genSSATrees(List(a, b), List(aTree, bTree)) :+ GT(id, top(aTree), top(bTree))
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
  case class TCall(retType: Type, name: String, params:List[TypedAST]) extends TypedAST
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
  case class TSqrt(x: TypedAST) extends TypedAST
  case class TLength(x: TypedAST) extends TypedAST
  case class TLT(a:TypedAST, b:TypedAST) extends TypedAST
  case class TGT(a:TypedAST, b:TypedAST) extends TypedAST
  case class TLoop(cond: List[TypedAST], compute: List[TypedAST], cont: List[TypedAST], merge: List[TypedAST]) extends TypedAST
  case class TNull(t: Type) extends TypedAST
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
    case TCall(retType, _, _) => retType
    case TVar(_, t) => t
    case TConst(_, t, _) => t
    case TNull(t) => t
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
    case TSqrt(a) => treeType(a)
    case TLength(a) => Float
    case TLT(a, b) => Bool

    case TLoop(_, _, _, _) => Void
  }
  
  def generateSAST(tree: TypedAST): strippedAST = tree match {
    case TAssign(n, v) => ASTAssign(n, generateSAST(v))
    case TName(n, t) => ASTName(n)
    case TFun(name, retType, params, pTypes, body) => ASTFun(name, retType, params, pTypes, ASTLabelAnon :: body.map(generateSAST))
    case TCall(retType, name, ps) => ASTCall(retType, name, ps.map(generateSAST))
    case TVar(n, t) => ASTVar(n, t)
    case TConst(n, t, l) => ASTConst(n, t, l)
    case TNull(t) => ASTNull(t)
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
    case TSqrt(x) => ASTSqrt(treeType(x), generateSAST(x))
    case TLength(x) => ASTLength(treeType(TLength(x)), generateSAST(x))
    case TLT(a, b) => ASTLt(generateSAST(a), generateSAST(b))
    case TGT(a, b) => ASTGt(generateSAST(a), generateSAST(b))
    case TLoop(cond, comp, cont, merge) => ASTLoop(cond.map(generateSAST), comp.map(generateSAST), cont.map(generateSAST), merge.map(generateSAST))
  }
}
