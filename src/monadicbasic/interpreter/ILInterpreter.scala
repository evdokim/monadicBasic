package monadicbasic.interpreter

import monadicbasic._
import java.util.HashMap
import java.util.Scanner

class ILInterpreter(ops: List[Op]) {
    private val labelsPointers = new HashMap[String, Int]()
    private val context = new HashMap[String, Value]()


  def execute {
    val scanner = new Scanner(System.in);
    calculateLabelsPointers
    println(labelsPointers)
    var p = 0
    var error = false
    var exitMessage = "program was exit normally"
    var i = 0

    def errorReport(errorType: String, pos:Pos, message:String) {
      error = true
      exitMessage = errorType + " at " + pos + " : " + message
    }

    def exprErrorReport(message:String) {
      error = true
      exitMessage = message
    }

    while (p < ops.size && !error) {
      i+=1
      //println("p=" + p + ": " + ops(p))

      ops(p) match {    

        case PrintOp(expr) => ExpressionInterpreter.eval(expr, context) match {
           case Error(s) => exprErrorReport(s)
           case v => println(v)                                 
        }

        case InputOp(name, pos) => context.get(name) match {
          case value:Value => value.typeName match {
            case "integer" => context.put(name, IntValue(scanner.nextInt))
            case "boolean" => context.put(name, BooleanValue(scanner.nextBoolean))
            case "string" => context.put(name, StringValue(scanner.nextLine))
            case typeName => 
                errorReport("type error", pos, typeName + " unsupported in INPUT")
          }
          case _ => 
            errorReport("runtime error", pos, "variable " + name + " wasn't defined")
        }

        case VarDefOp(varName, typeName, pos) => context.get(varName) match {
          case value:Value => 
            errorReport("runtime error", pos, 
                        "variable " + varName + " was defined already")
          case _ => context.put(varName, Types.getDefaultValue(typeName, pos))
        }         

        case ArrayDefOp(varName, typeName, sizeExpr, pos) => {
          context.get(varName) match {
            case value:Value => errorReport("runtime error ", pos,
                              "variable " + varName + " was defined already")
            case _ => ExpressionInterpreter.eval(sizeExpr, context) match {
              case IntValue(size) => 
                  context.put(varName, Types.getDefaultArrayValue(typeName, size, pos))
              case Error(s) => exprErrorReport(s)
              case value => errorReport("type error", pos, "index must be integer")
            }
          }
        }         

        case AssigmentOp(name, expr, pos) => {
          ExpressionInterpreter.eval(expr, context) match {       
            case Error(s) => exprErrorReport(s)
            case rightValue => context.get(name) match {
              case value:Value if (value.typeName == rightValue.typeName) => {
                context.put(name, rightValue) 
              }
              case Error(s) => exprErrorReport(s)
              case value:Value => errorReport("type error", pos, 
                  "required: " + value.typeName + " found: " + rightValue.typeName)
              case _ => errorReport("runtime error ", pos, 
                        "variable " + name + " wasn't defined")
            }
          }
        }

        case AssigmentArrayOp(name, index, expr, pos) => {
          ExpressionInterpreter.eval(expr, context) match {       
            case Error(s) => exprErrorReport(s)
            case rightValue => ExpressionInterpreter.eval(index, context) match {
              case IntValue(index) => context.get(name) match {
                case ArrayValue(array, typeName) if (typeName == rightValue.typeName) => {
                    if (index>0 && index<=array.size)
                      array(index - 1) =  rightValue 
                    else
                      errorReport("runtime error: ", pos, "out of bounds")
                }
                case Error(s) => exprErrorReport(s)
                case _ => errorReport("type error", pos, 
                            "required: " + rightValue.typeName)
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
            } else if(v) {
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
            } else if(!v) {
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


  private def calculateLabelsPointers {
    for(i <- 0 to (ops.size-1)) {
      ops(i) match {
        case LabelOp(name) => labelsPointers.put(name, i)
        case _ =>
      }     
    }
  }
}
