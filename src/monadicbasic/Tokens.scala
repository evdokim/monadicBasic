package monadicbasic

case class Pos(line: Int, column: Int) {
  override def toString = "line=" + line + " column=" + column
}

abstract sealed class Token

case class WordToken(s: String) extends Token {
  override def toString = s
}

case class NumberToken(n: Int) extends Token {
  override def toString = n.toString
}

case class StringLiteralToken(s: String) extends Token {
  override def toString = "\"" + s + "\""
}

case object NewLineToken extends Token {
  override def toString = "\\n"
}