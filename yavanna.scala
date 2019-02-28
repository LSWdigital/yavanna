package yavanna

import scala.collection.immutable.Map

import parser._
import flattener._
import strippedASTEnc._
import ssaEnc._
import spirvEnc._
import typedSSAGen._
import ssaGen._
import spirvGen._
import compilerPipeline._
import typedASTEnc._
import org.kiama.output.PrettyPrinter._
object yavanna {
  def main(args: Array[String]): Unit = {

    val X = "vec4 colour = +(*((1.0, 1.1, 1.3, 1.5), z), x).xzw;\ncolour = dot(colour, (x, y, z));"
    val Success(e, _) = parseAll(program, X)

    println("Program:")
    println(X)

    println("\n")

    println("AST:")
    println(pretty(any(e)))

    println("\n")

    println("Flattened AST:")
    println(pretty(any(flatten(e))))

    //Aaaahhh

    val Y = ASTArith(Float, OpFAdd, ASTName("a"), ASTName("b"))
    println("Stripped AST:")
    println(pretty(any(Y)))
    
    println("\n")
    println("Typed SSA:")

    val YID = genIDTree(Y, 0) match {
      case(tree, _) => tree
    }
    println(pretty(any(genSSATree(Y, YID))))

    println("\n")
    println("Untyped SSA:")

    val m = scala.collection.immutable.Map[Type, ID]()
    val CT = generateSSA(genSSATree(Y, YID), m, Nil, Nil) match {
      case(c, map, t) => t ++ c
    }

    println(pretty(any(CT)))

    
    println("\n")
    println("SpirV:")
    println((CT.map(lineGen).flatten))

    println("\n")
    println("Code:")
    val fullGen = compileFromTypedTree( List( TConst("white", Float, FloatLiteral(1.0f)),  TFun("main", Void, Nil, Nil, List( TLabel("start"), TStore("outColor", TConstruct( List(TName("white", Float), TName("white", Float), TName("white", Float), TName("white", Float)))), TReturn))))
    println(fullGen)
  }
}

import strippedASTGen._
object compilerPipeline {
  def typeTreeToSSA(t: TypedAST): (List[SSA], List[SSA]) = {   
    val sTree  = generateSAST(t)
    val idTree = genIDTree(sTree, 0) match {
      case (tree, _) => tree
    }
    val tSSA   = genSSATree(sTree, idTree)

    val ssa = generateSSA(tSSA, scala.collection.immutable.Map[Type, ID](), Nil, Nil) match {
      case (c, m, t) => (c, t)
    }

    return ssa
  }

  def twoListstoOne(ssas: List[(List[SSA], List[SSA])]) : (List[SSA], List[SSA]) = ssas match {
    case Nil => (Nil, Nil)
    case (c, t)::tail => twoListstoOne(tail) match {
      case (Nil, Nil) => (c, t)
      case (d, s) => (c ++ d, t ++ s)
    }
  }

  def compileFromTypedTree(t: List[TypedAST]): String = {
    val ssas = t.map(typeTreeToSSA)
    
    val ssa = twoListstoOne(ssas) match {
      case (cs, ts) => ts ++ List(
          Assign(Id("v4_output"), OpTypePointer, List(Output, Id(Vector(Float, 4).toString))),
          Assign(Id("v4_input"),  OpTypePointer, List(Input,  Id(Vector(Float, 4).toString))),
          Assign(Id("outColor"),  OpVariable, List(Id("v4_output"), Output)),
          Assign(Id("fragColor"), OpVariable, List(Id("v4_input"),  Input))
        ) ++ cs.distinct
    }

    val ls = ssa.map(lineGen).flatten

    return genShader(ls)
  }
}
