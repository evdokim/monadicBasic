package monadicbasic

import monadicbasic.parsers._
import java.util.Scanner

import monadicbasic.interpreter._

object Main {
  def main(args:Array[String]) = {
    println("begin")
    val sc = new Scanner(System.in);
    val in  = new StringBuilder();
    while(sc.hasNext) {
      in.append(sc.nextLine() + "\n");
    }

    val tokens = Tokenizer.tokenize(in.toList)
    tokens match {
      case Left(error) => println(error)
      case Right(list) => {
        println(list)
        BasicParsers.blockStatement(list) match {
          case Failure(s) => println(s)
          case FatalFailure(s) => println(s)
          case Success(ast, rem) => {           
            println(ast)
            val ops = Translator.translate(ast)
            for(i<- 0 to (ops.size-1)) {
              println(i +": " + ops(i))
            }
            println

            val interpreter = new ILInterpreter(ops)          
            interpreter.execute
          }
        }
      }
    }
  }
}

