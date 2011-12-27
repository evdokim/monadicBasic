package monadicbasic.interpreter

import monadicbasic._
import java.util.HashMap
import java.util.Scanner

class ILInterpreter(ops: List[Op]) {
  private val labelsPointers = new HashMap[String, Int]()

  def execute(verbose: Boolean) {
    val context = new HashMap[String, Value]()
    val scanner = new Scanner(System.in);
    calculateLabelsPointers
    if (verbose) println(labelsPointers)
    var p = 0
    var error = false
    var exitMessage = "program was exit normally"
    var i = 0

    def errorReport(errorType: String, pos: Pos, message: String) {
      error = true
      exitMessage = errorType + " at " + pos + " : " + message
    }

    def exprErrorReport(message: String) {
      error = true
      exitMessage = message
    }

    while (p < ops.size && !error) {
      i += 1

      ops(p) match {

        case PrintOp(expr) => ExpressionInterpreter.eval(expr, context) match {
          case Error(s) => exprErrorReport(s)
          case v => println(v)
        }

        case InputOp(name, pos) => context.get(name) match {
          case value: Value => value.getType match {
            case IntegerType => context.put(name, IntValue(scanner.nextInt))
            case BooleanType => context.put(name, BooleanValue(scanner.nextBoolean))
            case StringType => context.put(name, StringValue(scanner.next))
            case otherType =>
              errorReport("type error", pos, otherType + " unsupported in INPUT")
          }
          case _ =>
            errorReport("runtime error", pos, "variable " + name + " wasn't defined")
        }

        case VarDefOp(varName, typeInfo) => {
          context.put(varName, Types.getDefaultValue(typeInfo))
        }

        case ArrayDefOp(varName, typeInfo, sizeExpr, pos) => {
          ExpressionInterpreter.eval(sizeExpr, context) match {
            case IntValue(size) if (size > 0) =>
              context.put(varName, Types.getDefaultArrayValue(typeInfo, size))
            case IntValue(_) => errorReport("type error", pos, "array size must be positive")
            case Error(s) => exprErrorReport(s)
            case value => errorReport("type error", pos, "index must be integer")
          }
        }

        case AssigmentOp(name, expr, pos) => {
          ExpressionInterpreter.eval(expr, context) match {
            case Error(s) => exprErrorReport(s)
            case rightValue => context.get(name) match {
              case value: Value if (value.getType == rightValue.getType) => {
                context.put(name, rightValue)
              }
              case Error(s) => exprErrorReport(s)
              case value: Value => errorReport("type error", pos,
                "required: " + value.getType + " found: " + rightValue.getType)
              case _ => errorReport("runtime error ", pos,
                "variable " + name + " wasn't defined")
            }
          }
        }

        case AssigmentArrayOp(name, index, expr, pos) => {
          ExpressionInterpreter.eval(expr, context) match {
            case Error(s) => exprErrorReport(s)
            case rightValue => ExpressionInterpreter.eval(index, context) match {
              case IntValue(indexValue) => context.get(name) match {
                case ArrayValue(array, typeInfo) if (typeInfo == rightValue.getType) => {
                  if (indexValue > 0 && indexValue <= array.size)
                    array(indexValue - 1) = rightValue
                  else
                    errorReport("runtime error: ", pos, "out of bounds")
                }
                case Error(s) => exprErrorReport(s)
                case _ => errorReport("type error", pos,
                  "required: " + rightValue.getType)
              }
              case Error(s) => exprErrorReport(s)
              case _ => errorReport("type error", pos, "index must be integer")
            }
          }
        }

        case JumpOp(target, pos) => {
          if (!labelsPointers.containsKey(target)) {
            errorReport("runtime error", pos, "couldn't found label" + target)
          } else {
            p = labelsPointers.get(target)
          }
        }

        case JumpIfOp(pred, target, pos)
        => ExpressionInterpreter.eval(pred, context) match {
          case Error(s) => exprErrorReport(s)
          case BooleanValue(v) => {
            if (!labelsPointers.containsKey(target)) {
              errorReport("runtime error", pos, "couldn't found label" + target)
            } else if (v) {
              p = labelsPointers.get(target)
            }
          }
          case _ => errorReport("type error", pos, "required: boolean")
        }

        case JumpIfNotOp(pred, target, pos)
        => ExpressionInterpreter.eval(pred, context) match {
          case Error(s) => exprErrorReport(s)
          case BooleanValue(v) => {
            if (!labelsPointers.containsKey(target)) {
              errorReport("runtime error", pos, "couldn't found label" + target)
            } else if (!v) {
              p = labelsPointers.get(target)
            }
          }
          case _ => errorReport("type error", pos, "required: boolean")
        }

        case LabelOp(_) => {}

      }
      p += 1
    }
    println(exitMessage)
  }


  private def calculateLabelsPointers() {
    for (i <- 0 to (ops.size - 1)) {
      ops(i) match {
        case LabelOp(name) => labelsPointers.put(name, i)
        case _ =>
      }
    }
  }
}
