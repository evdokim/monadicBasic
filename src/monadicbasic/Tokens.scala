package monadicbasic

case class Pos(line:Int, column: Int) {
	override def toString = "line=" + line + " column=" + column
}

abstract sealed class Token
  case class WordToken(s:String) extends Token
  case class NumberToken(n:Int) extends Token
  case class StringLiteralToken(s:String) extends Token
  case object NewLineToken extends Token