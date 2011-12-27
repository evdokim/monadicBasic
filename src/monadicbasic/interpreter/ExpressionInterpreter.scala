package monadicbasic.interpreter

import monadicbasic._
import java.util.HashMap

object ExpressionInterpreter {
	def eval(expr: Expression, env: HashMap[String, Value]): Value = expr match {
		case Less(expr1, expr2) => (eval(expr1, env), eval(expr2, env)) match {
		    case (e @ Error(s), _) => e
		    case (_, e @ Error(s)) => e
		    case (IntValue(v1), IntValue(v2)) => new BooleanValue(v1 < v2)
		    case _ => new Error("type error")
		}

		case LE(expr1, expr2) => (eval(expr1, env), eval(expr2, env)) match {
		    case (e @ Error(s), _) => e
		    case (_, e @ Error(s)) => e
		    case (IntValue(v1), IntValue(v2)) => new BooleanValue(v1 <= v2)
		    case _ => new Error("type error")
		}

		case Greater(expr1, expr2) => (eval(expr1, env), eval(expr2, env)) match {
		    case (e @ Error(s), _) => e
		    case (_, e @ Error(s)) => e
		    case (IntValue(v1), IntValue(v2)) => new BooleanValue(v1 > v2)
		    case _ => new Error("type error")
		}

		case GE(expr1, expr2) => (eval(expr1, env), eval(expr2, env)) match {
		    case (e @ Error(s), _) => e
		    case (_, e @ Error(s)) => e
		    case (IntValue(v1), IntValue(v2)) => new BooleanValue(v1 >= v2)
		    case _ => new Error("type error")
		}
		case Equal(expr1, expr2) => (eval(expr1, env), eval(expr2, env)) match {
		    case (e @ Error(s), _) => e
		    case (_, e @ Error(s)) => e
		    case (IntValue(v1), IntValue(v2)) => new BooleanValue(v1 == v2)
		    case (StringValue(v1), StringValue(v2)) => new BooleanValue(v1 == v2)
		    case (BooleanValue(v1), BooleanValue(v2)) => new BooleanValue(v1 == v2)
		    case _ => new Error("type error")
		}

		case NotEqual(expr1, expr2) => (eval(expr1, env), eval(expr2, env)) match {
		    case (e @ Error(s), _) => e
		    case (_, e @ Error(s)) => e
		    case (IntValue(v1), IntValue(v2)) => new BooleanValue(v1 != v2)
		    case (StringValue(v1), StringValue(v2)) => new BooleanValue(v1 != v2)
		    case (BooleanValue(v1), BooleanValue(v2)) => new BooleanValue(v1 != v2)
		    case _ => new Error("type error")
		}

		case Add(expr1, expr2) => (eval(expr1, env), eval(expr2, env)) match {
		    case (e @ Error(s), _) => e
		    case (_, e @ Error(s)) => e
		    case (IntValue(v1), IntValue(v2)) => new IntValue(v1 + v2)
		    case (StringValue(v1), v2) => new StringValue(v1 + v2.toString)
		    case (v1, StringValue(v2)) => new StringValue(v1.toString + v2)
		    case _ => new Error("type error")
		  
		}

		case Sub(expr1, expr2)  => (eval(expr1, env), eval(expr2, env)) match {
		    case (e @ Error(s), _) => e
		    case (_, e @ Error(s)) => e
		    case (IntValue(v1), IntValue(v2)) => new IntValue(v1 - v2)
		    case _ => new Error("type error")
		}

		case Neg(expr) => eval(expr, env) match {
		    case e @ Error(s) => e
		    case IntValue(v) => new IntValue(-v)
		    case _ => new Error("type error")
		}


		case Mult(expr1, expr2) => (eval(expr1, env), eval(expr2, env)) match {
		    case (e @ Error(s), _) => e
		    case (_, e @ Error(s)) => e
		    case (IntValue(v1), IntValue(v2)) => new IntValue(v1 * v2)
		    case _ => new Error("type error")
		}

		case Div(expr1, expr2) => (eval(expr1, env), eval(expr2, env)) match {
		    case (e @ Error(s), _) => e
		    case (_, e @ Error(s)) => e
		    case (IntValue(v1), IntValue(v2)) => new IntValue(v1 / v2)
		    case _ => new Error("type error")
		}

		case Var(name, None) => env.get(name) match {
			case value:Value => value
			case _ => Error("undefined variable: " + name + "!")
		}
		  
		case Var(name, Some(indexExpr)) => env.get(name) match {
			case ArrayValue(array, _) => eval(indexExpr, env) match {
				case IntValue(index) =>
				  if (index>0 && index<=array.size)
			        array(index - 1)  
			      else
			        Error("out of bound")
			    case e @ Error(_) => e
			    case _ => Error("type error")
			}

			case _ => Error("undefined variable: " + name + "!")
		}

		case NumberLiteral(value) => IntValue(value)
		case StringLiteral(value) => StringValue(value)
		case BooleanLiteral(value) => BooleanValue(value)
		


	} 
	
}

