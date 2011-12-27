package monadicbasic

import scala.collection.mutable.ListBuffer

object Tokenizer {

  object CurrentTokenType extends Enumeration {
    type currentTokenTypeToken = Value
    val Undef, Word, Number, StringLiteral, NewLine, Op = Value
  }

  def tokenize(in: List[Char]): Either[String, List[(Token, Pos)]] = {
    import CurrentTokenType._

    var result: Either[String, List[(Token, Pos)]] = Left("")
    val resultList = new ListBuffer[(Token, Pos)]()
    val twoCharOp = Set("<=", "<>", ">=")
    var currentTokenType = Undef
    var buffer = new ListBuffer[Char]()
    var (curLine, curColumn) = (0, 0)
    var (lastLine, lastColumn) = (0, 0)
    var error = false
    var p = -1

    def closeToken() {
      //println("close " + currentTokenType)
      val token = buffer.mkString
      val resultToken: Token = currentTokenType match {
        case Number => NumberToken(token.toInt)
        case Undef => {
          println("error");
          WordToken("error")
        }
        case NewLine => NewLineToken
        case Word => WordToken(token.toLowerCase)
        case StringLiteral => StringLiteralToken(token)
        case Op => WordToken(token)
      }
      resultList.append((resultToken, Pos(lastLine, lastColumn)))
      buffer = new ListBuffer[Char]()
      currentTokenType = Undef
    }

    def openTocken(ctt: Value) {
      //println("open " + ctt)
      lastLine = curLine
      lastColumn = curColumn
      currentTokenType = ctt
    }

    def errorReport() {
      error = true
      result = Left("unexpected " + in(p) + " at line: " + curLine + " column: " + curColumn)
    }

    def errorReportDetailed(expected: String) {
      error = true
      result = Left("at line:" + curLine + " column: " + curColumn + " expected: " +
        expected + " found: " + in(p))
    }

    while (p < in.size - 1 && !error) {
      p += 1
      val c = in(p)
      c match {
        case d if Character.isDigit(d) => {
          currentTokenType match {
            case Undef => {
              openTocken(Number)
              buffer.append(d)
            }
            case Number | StringLiteral | Word => buffer.append(d)
            case Op | NewLine => {
              closeToken()
              openTocken(Number)
              buffer.append(d)
            }
          }
          curColumn += 1
        }
        case '<' | '>' | '(' | ')' | '+' | '-' | '*' | '/' | '=' | ':' => {
          currentTokenType match {
            case Undef => {
              openTocken(Op)
              buffer.append(c)
            }
            case Word | Number | NewLine => {
              closeToken()
              openTocken(Op)
              buffer.append(c)
            }
            case Op => {
              if (twoCharOp.contains(buffer.mkString + c)) {
                buffer.append(c)
              } else {
                closeToken()
                openTocken(Op)
                buffer.append(c)
              }
            }
            case StringLiteral => {
              buffer.append(c)
            }
          }
          curColumn += 1
        }

        case '\n' => {
          currentTokenType match {
            case StringLiteral => errorReportDetailed("\"")
            case Undef => openTocken(NewLine)
            case Op if (buffer(0) != ')' && buffer(0) != ':') => errorReportDetailed("operand")
            case Word | NewLine | Number | Op => {
              closeToken()
              openTocken(NewLine)
            }
          }
          curColumn = 0
          curLine += 1
        }

        case '"' => {
          currentTokenType match {
            case Undef => openTocken(StringLiteral)
            case StringLiteral => closeToken()
            case Op | Word | NewLine => {
              closeToken()
              openTocken(StringLiteral)
            }
            case Number => errorReportDetailed("number")
          }
          curColumn += 1
        }

        case ' ' | '\t' => {
          currentTokenType match {
            case StringLiteral => {
              buffer.append(c)
            }
            case Undef => {}
            case Op | Number | NewLine | Word => closeToken()
          }
          curColumn += 1
        }

        case l if Character.isLetter(l) => {
          currentTokenType match {
            case Undef => {
              openTocken(Word)
              buffer.append(l)
            }
            case Word | StringLiteral => {
              buffer.append(l)
            }
            case Number => errorReportDetailed("number")
            case Op | NewLine => {
              closeToken()
              openTocken(Word)
              buffer.append(l)
            }
          }
          curColumn += 1
        }

        case c => currentTokenType match {
          case StringLiteral => buffer.append(c)
          case _ => errorReport()
        }
      }
    }
    if (!error) {
      currentTokenType match {
        case StringLiteral => errorReportDetailed("\"")
        case other => {
          closeToken()
        }
      }
    }

    if (!error) {
      Right(resultList.toList)
    } else {
      result
    }

  }
}
