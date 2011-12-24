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
		    case (StringValue(v1), StringValue(v2)) => new StringValue(v1 + v2)
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

		case Var(name) => {
		   if (env.containsKey(name)) {
		      env.get(name)
		    } else {
		      Error("undefined variable: " + name + "!")
		    }
		  
		}

		case NumberLiteral(value) => IntValue(value)
		


	} 
	
}
