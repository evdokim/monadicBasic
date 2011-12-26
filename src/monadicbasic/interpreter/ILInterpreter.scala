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
	  while (p < ops.size && !error) {
	    i+=1
	  	//println("p=" + p + ": " + ops(p))

	  	ops(p) match { 	  


	  		case PrintOp(expr) => ExpressionInterpreter.eval(expr, context) match {
	  			 case Error(s) => {error = true; exitMessage = s}
	  			 case v => println(v)                                 
	  		}

	  		case InputOp(name) => {
	  			if (!context.containsKey(name)) {
		  			  error = true
	  				  exitMessage = "variable " + name + " wasn't defined"	 
		  			 
		  		} else context.get(name).typeName match {
		  			case "Integer" => context.put(name, IntValue(scanner.nextInt))
		  			case "Boolean" => context.put(name, BooleanValue(scanner.nextBoolean))
		  			case "String" => context.put(name, StringValue(scanner.nextLine))
		  			case typeName => {
			  			error=true
			  			exitMessage = "type " + typeName + " unsupported in INPUT"
			  		}	  			
	  		       
	  		    }
	  		}

	  		case VarDefOp(varName, typeName) => {
	  			if(!context.containsKey(varName)) {
	  				context.put(varName, Types.getDefaultValue(typeName))
	  			} else {
	  				error = true
	  				exitMessage = "variable " + varName + " was defined already"
	  			}
	  		}

	  	    case ArrayDefOp(varName, typeName, sizeExpr) => {
	  			if(!context.containsKey(varName)) {
	  				ExpressionInterpreter.eval(sizeExpr, context) match {
	  				  case IntValue(size) => 
	  				    context.put(varName, Types.getDefaultArrayValue(typeName, size))
	  				  case Error(msg) => {error = true; exitMessage = msg}
	  				  case value => {error = true; exitMessage = "integer value expected, found: " + value}
	  				}
	  			} else {
	  				error = true
	  				exitMessage = "variable " + varName + " was defined already"
	  			}
	  		}

	  		case AssigmentOp(name, expr) => ExpressionInterpreter.eval(expr, context) match {  			
	  			case Error(s) => {error = true; exitMessage = s} 
	  			case value => 
		  			if (!context.containsKey(name)) {
		  			  error = true
	  				  exitMessage = "variable " + name + " wasn't defined"	 
		  			 
		  			} else if(context.get(name).typeName == value.typeName) {
		  			   context.put(name, value)	
			  		} else {
			  			error = true
			  			exitMessage = "type error"
			  		}
	  		}

	  		case AssigmentArrayOp(name, index, expr) => ExpressionInterpreter.eval(expr, context) match {  			
	  			case Error(s) => {error = true; exitMessage = s} 
	  			case value => context.get(name) match {
	  				case ArrayValue(array, typeName) if (typeName==value.typeName) => {
	  					ExpressionInterpreter.eval(index, context) match {
	  						case IntValue(index) =>
							  if (index>0 && index<=array.size)
						        array(index - 1) =  value 
						      else
						        Error("out of bound")
						    case e @ Error(_) => e
						    case _ => Error("type error")
	  					}
	  				}
	  				case _ => {error = true; exitMessage = "typeError"}    
	  				}
	  			}
		  			
	  		

	  		case JumpOp(target) => p = labelsPointers.get(target)

	  		case JumpIfOp(pred, target) => ExpressionInterpreter.eval(pred, context) match {
	  			case Error(s) => {error = true; exitMessage = s}
	  			case BooleanValue(v) => if (v) p = labelsPointers.get(target)
	  			case _ => {error = true; exitMessage = "type error"}
	  		}

	  		case JumpIfNotOp(pred, target) => ExpressionInterpreter.eval(pred, context) match {
	  			case Error(s) => {error = true; exitMessage = s}
	  			case BooleanValue(v) => if (!v) p = labelsPointers.get(target)
	  			case _ => {error = true; exitMessage = "type error"}
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
