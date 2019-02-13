package yavanna

import parser._
import flattener._
import continuator._
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

    println("Instruction List:")
    println(pretty(any(continue(flatten(e)))))
  }
}
