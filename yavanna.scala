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

    val X = "const zero 0.0; const one 1.0; const two 2.0; const depth 50.0; " ++ 
    "fun cmult(a|Vec2, b|Vec2) Vec2 {complex|Vec2 = Vec2(*(get a.X, get b.X), -(zero, *(get a.Y, get b.Y))); return(complex)}; " ++
    "fun chooseCol(col|Float, prox|Float, black|Vec2) Float when gt(length(black), two) is prox else chooseCol(col, col, Vec2(two, two)); " ++
    "fun mandelbrot(z|Vec2, c|Vec2, d|Float, result|Float) Float when lt(d, zero) is col else mandelbrot(+(cmult(z, z), c), c, -(d, one), chooseCol(result, d, z)); " ++
    "fun main() Unit {coord|Vec2 = Vec2(get !fragColor.X, get !fragColor.Y); color|Float = mandelbrot(coord, coord, depth, zero); outColor := Vec4(color, color, color, one); return}"

    val Success(e, _) = parseAll(program, X)

    println("Program:")
    println(X)

    println("\n")

    println("AST:")
    println(pretty(any(e)))

    println("\n")

    
    val programString = expressionsToTypedAST(e, (scala.collection.immutable.Map[String, Type]() + ("fragColor" -> Vector(Float, 3)) + ("outColor" -> Vector(Float, 4)) )) match {
      case (ast, _) => compileFromTypedTree(ast)
    }

    println(programString)
  }
}

import strippedASTGen._
object compilerPipeline {
  def typeTreeToSSA(t: TypedAST, i: Int): ((List[SSA], List[SSA]), Integer) = {   
    val sTree  = generateSAST(t)
    val (idTree, int) = genIDTree(sTree, i)
    val tSSA   = genSSATree(sTree, idTree)

    val ssa = generateSSA(tSSA, scala.collection.immutable.Map[Type, ID](), Nil, Nil) match {
      case (c, m, t) => ((c, t), int)
    }

    return ssa
  }

  def typeTreesToSSA(t: List[TypedAST], int: Integer): (List[(List[SSA], List[SSA])], Integer) = t match {
    case Nil => (Nil, int)
    case h::t => typeTreeToSSA(h, int) match {
      case (head, i) => typeTreesToSSA(t, i) match {
        case(tail, j) => (head::tail, j)
      }
    } 
  }

  def twoListstoOne(ssas: List[(List[SSA], List[SSA])]) : (List[SSA], List[SSA]) = ssas match {
    case Nil => (Nil, Nil)
    case (c, t)::tail => twoListstoOne(tail) match {
      case (Nil, Nil) => (c, t)
      case (d, s) => (c ++ d, t ++ s)
    }
  }

  def compileFromTypedTree(t: List[TypedAST]): String = {
    val ssas = typeTreesToSSA(t, 0) match {
      case (a, _) => a
    }
    
    val ssa = twoListstoOne(ssas) match {
      case (cs, ts) => (ts ++ List(
          makeType(Id(Vector(Float, 3).toString), Vector(Float, 3)),
          Assign(Id("v4_output"), OpTypePointer, List(Output, Id(Vector(Float, 4).toString))),
          Assign(Id("v3_input"),  OpTypePointer, List(Input,  Id(Vector(Float, 3).toString))),
          Assign(Id("outColor"),  OpVariable, List(Id("v4_output"), Output)),
          Assign(Id("fragColor"), OpVariable, List(Id("v3_input"),  Input))
        )).distinct ++ cs
    }

    val ls = ssa.map(lineGen).flatten

    return genShader(ls)
  }
}
