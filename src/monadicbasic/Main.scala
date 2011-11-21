package monadicbasic
import monadicbasic.parsers._
import java.util.Scanner

object Main {
  def main(args:Array[String]) = {
      val sc = new Scanner(System.in);
      val in  = new StringBuilder();
      while(sc.hasNext()){
        in.append(sc.nextLine() + "\n");
      }

    BasicParsers.statements(in.toList) match {
        case Success(list, rem) =>
            //println(list + " " + rem) 
           println(list.foldLeft(Map[String, Int]())((map, statement) => statement.execute(map)))
        case a => println(a)
    }

  }
}

