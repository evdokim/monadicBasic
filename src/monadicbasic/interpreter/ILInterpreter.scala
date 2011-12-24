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
	  while (p < ops.size && !error && (i<100)) {
	    i+=1
	  	println("p=" + p + ": " + ops(p))

	  	ops(p) match {

	  		case PrintOp(expr) => ExpressionInterpreter.eval(expr, context) match {
	  			 case Error(s) => {error = true; exitMessage = s}
	  			 case IntValue(v) => println(v)
                 case StringValue(v) => println(v)
                 case BooleanValue(v) => println(v)                 
	  		}

	  		case InputOp(name) => {
	  			context.put(name, IntValue(scanner.nextInt))
	  		}

	  		case AssigmentOp(name, expr) => ExpressionInterpreter.eval(expr, context) match {  			
	  			case Error(s) => {error = true; exitMessage = s} 
	  			case v:Value => context.put(name, v)
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
