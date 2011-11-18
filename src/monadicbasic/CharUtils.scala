package monadicbasic

object CharUtils {
  def isDigit(c:Char) = ('0' <= c) && (c <= '9')
  def isLcLetter(c:Char) = ('a' <= c) && (c <= 'z')
  def isUcLetter(c:Char) = ('A' <= c) && (c <= 'Z')
  def isLetter(c:Char) = isLcLetter(c) || isUcLetter(c)

  def charsToString(l:List[Char]) = l.foldLeft("")({_+_})
}