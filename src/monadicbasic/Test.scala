package monadicbasic
import monadicbasic.parsers._
import java.util.Scanner

object Test {
  def main(args:Array[String]) = {
      val sc = new Scanner(System.in);
      
      while(sc.hasNext()){
        println(BasicParsers.literal("x").rep(sc.nextLine.toList))
      }

    

  }
}

