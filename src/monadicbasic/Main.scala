package monadicbasic

import monadicbasic.parsers._
import java.util.Scanner

import monadicbasic.interpreter._;

object Main {
  def main(args:Array[String]) = {
      val sc = new Scanner(System.in);
      val in  = new StringBuilder();
      while(sc.hasNext) {
        in.append(sc.nextLine() + "\n");
      }

    val r = BasicParsers.blockStatement(in.toList)
    println(r)

    var ops = List[Op]()
    
    r match {
      case Success(statement, _) => {
        ops = (new Compiler()).compile(statement)
      }
      case _ => 
    }
    for(i<- 0 to (ops.size-1)) {
      println(i +": " + ops(i))
    }
    println
    val interpreter = new ILInterpreter(ops)

    interpreter.execute





   
    //println(r.map(_.execute(Map())))

    // r match {
    //     case Success(list, rem) =>
    //         //println(list + " " + rem) 
    //        println(list.foldLeft(Map[String, Int]())((map, statement) => statement.execute(map)))
    //     case a => println(a)
    // }

  }
}

