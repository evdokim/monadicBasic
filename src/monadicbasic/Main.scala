package monadicbasic

import monadicbasic.parsers._
import java.util.Scanner

import monadicbasic.interpreter._
import java.io.{File, FileInputStream}

object Main {
  def main(args: Array[String]) {
    println("monadic basic interpreter v 1.0")
    val verbose = if (args.size < 2) false else (args(1) == "-verbose")
    val sc = new Scanner(if (args.size == 0) System.in else new FileInputStream(new File(args(0))));
    val in = new StringBuilder();
    while (sc.hasNext) {
      in.append(sc.nextLine() + "\n");
    }
    if (verbose) println("reading is finished")
    sc.close();

    val tokens = Tokenizer.tokenize(in.toList)
    tokens match {
      case Left(error) => println(error)
      case Right(list) => {
        if (verbose) println(list)
        BasicParsers.blockStatement(list) match {
          case Failure(s) => println(s)
          case FatalFailure(s) => println(s)
          case Success(ast, rem) => {
            if (verbose) println(ast)
            val ops = Translator.translate(ast)
            for (i <- 0 to (ops.size - 1)) {
              if (verbose) println(i + ": " + ops(i))
            }
            if (verbose) println()

            val interpreter = new ILInterpreter(ops)
            interpreter.execute(verbose)
          }
        }
      }
    }
  }
}

