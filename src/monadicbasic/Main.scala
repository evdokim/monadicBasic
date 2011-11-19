package monadicbasic
import monadicbasic.parsers._

object Main {
  def main(args:Array[String]) = {
    println(BasicParsers.expr(args(0).toList).map(_.eval(Map())))
  }
}

