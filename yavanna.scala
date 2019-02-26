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

  }
}
