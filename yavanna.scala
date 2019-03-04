package yavanna

import scala.collection.immutable.Map

import parser._
import strippedASTEnc._
import ssaEnc._
import spirvEnc._
import typedSSAGen._
import ssaGen._
import spirvGen._
import compilerPipeline._
import typedASTEnc._
import typeGen._
import org.kiama.output.PrettyPrinter._
object yavanna {
  def main(args: Array[String]): Unit = {

    val X = "const pointone 0.1; const one 1.0; fun tailCall(i|Float, x|Vec4) Vec4 when lt(i, pointone) is x else tailCall(-(i, one), x); fun main() Unit {outColor := Vec4(one, one, one, one)}"
    val Success(e, _) = parseAll(program, X)

    println("Program:")
    println(X)

    println("\n")

    println("AST:")
    println(pretty(any(e)))

    println("\n")

    val fullGen = compileFromTypedTree( List( 
      TConst("one", Float, FloatLiteral(1.0f)),
      TConst("pointOne", Float, FloatLiteral(0.1f)),
      TConst("nil", Float, FloatLiteral(0.0f)),
      TConst("ten", Float, FloatLiteral(10.0f)),

      TFun("main", Void, Nil, Nil, List( 
        TVar("col", Vector(Float, 4)),
        TVar("loopedy", Float),
        TStore("col", TConstruct(List(
          TName("nil", Float),
          TName("nil", Float),
          TName("nil", Float),
          TName("nil", Float)
        ))),
        TStore("loopedy", TName("ten", Float)),
        
        TLoop(
          List(
            TLT(
              TLoad(Float, "loopedy"),
              TName("pointOne", Float)
            )
          ),
          List(
            TStore("col", TAdd(
                TLoad(Vector(Float, 4), "col"),
                TConstruct(
                  List(
                    TName("pointOne", Float),
                    TName("pointOne", Float),
                    TName("pointOne", Float),
                    TName("pointOne", Float)
                  )
                )
              )
            )
          ),
          List(
            TStore("loopedy", TSub(
                TLoad(Float, "loopedy"),
                TName("one", Float)
              )
            )
          ),
          List(TStore("outColor",
              TLoad(Vector(Float, 4), "col")
            )
          )
        ),

        TReturn))))
    
    val programString = expressionsToTypedAST(e, scala.collection.immutable.Map[String, Type]()) match {
      case (ast, _) => compileFromTypedTree(ast)
    }

    println(programString)
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
      case (cs, ts) => ts.distinct ++ List(
          makeType(Id(Vector(Float, 3).toString), Vector(Float, 3)),
          Assign(Id("v4_output"), OpTypePointer, List(Output, Id(Vector(Float, 4).toString))),
          Assign(Id("v3_input"),  OpTypePointer, List(Input,  Id(Vector(Float, 3).toString))),
          Assign(Id("outColor"),  OpVariable, List(Id("v4_output"), Output)),
          Assign(Id("fragColor"), OpVariable, List(Id("v3_input"),  Input))
        ) ++ cs
    }

    val ls = ssa.map(lineGen).flatten

    return genShader(ls)
  }
}
