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

    //val X = "const zero 0.0; const one 1.0; const two 2.0; const depth 50.0; " ++ 
    //"fun cmult(a|Vec2, b|Vec2) Vec2 {complex|Vec2 = Vec2(*(get a.X, get b.X), -(zero, *(get a.Y, get b.Y))); return(complex)}; " ++
    //"fun chooseCol(col|Float, prox|Float, black|Vec2) Float when gt(length(black), two) is prox else chooseCol(col, col, Vec2(two, two)); " ++
    //"fun mandelbrot(z|Vec2, c|Vec2, d|Float, result|Float) Float when lt(d, zero) is result else mandelbrot(+(cmult(z, z), c), c, -(d, one), chooseCol(result, d, z)); " ++
    //"fun main() Unit {coord|Vec2 = Vec2(get !fragColor.X, get !fragColor.Y); color|Float = mandelbrot(coord, coord, depth, zero); outColor := Vec4(color, color, color, one); return}"

    val Y = "const zero 0.0; const one 1.0; const two 2.0; const depth 50.0; "++
    "fun cmult(a|Vec2, b|Vec2) Vec2 { " ++
      "return( Vec2(" ++ 
        "-(*(get a.X, get b.X), *(get a.Y, get b.Y))," ++
        "+(*(get a.X, get b.Y), *(get a.Y, get b.X))" ++ 
      "))" ++
    "}; " ++
    "fun ifGT(big|Float, small|Float, x|Float, y|Float) Float " ++ 
      "when gt(big, small) is " ++
       "x " ++
      "else ifGT(+(small, one), big, y, x); " ++
    "fun ifGTc(large|Float, little|Float, v1|Vec2, v2|Vec2) Vec2 " ++
      "when gt(large, little) is " ++
        "v1 " ++ 
      "else ifGTc(+(little, one), large, v2, v1); " ++
    "fun mandelbrot(z|Vec2, c|Vec2, d|Float, result|Float) Float " ++
      "when gt(depth, d) is " ++
        "result " ++
      "else mandelbrot(+(cmult(z, z), c), c, +(d, one), ifGT(length(z), two, ifGT(result, zero, result, /(d, depth)), result)); " ++
    "fun main() Unit {" ++
      "coord|Vec2 = Vec2(*(get !fragColor.X, two), *(get !fragColor.Y, two)); " ++
      "color|Float = mandelbrot(coord, coord, zero, zero); " ++
      "outColor := Vec4(color, color, color, one); " ++
      "return " ++
    "}"

    val T = "const half 0.5; const one 1.0; const zero 0.0; fun square(a|Float, c|Float) Float when gt(a, half) is c else square(one, zero); fun main() Unit { outColor := Vec4(square(get !fragColor.X, one), square(get !fragColor.Y, one), zero, one); return }"

    val Success(e, _) = parseAll(program, T)

    println("Program:")
    println(T)

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
          makeType(Id(Bool.toString), Bool),
          Assign(Id("v4_output"), OpTypePointer, List(Output, Id(Vector(Float, 4).toString))),
          Assign(Id("v3_input"),  OpTypePointer, List(Input,  Id(Vector(Float, 3).toString))),
          Assign(Id("outColor"),  OpVariable, List(Id("v4_output"), Output)),
          Assign(Id("fragColor"), OpVariable, List(Id("v3_input"),  Input)),
          Assign(Id("false"), OpConstantFalse, List(Id("Bool")))
        )).distinct ++ cs
    }

    val ls = ssa.map(lineGen).flatten

    return genShader(ls)
  }
}
