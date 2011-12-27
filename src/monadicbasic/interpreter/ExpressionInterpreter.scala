package monadicbasic.interpreter

import monadicbasic._
import java.util.HashMap

object ExpressionInterpreter {
  def eval(expr: Expression, env: HashMap[String, Value]): Value = expr match {
    case Less(expr1, expr2, pos) => (eval(expr1, env), eval(expr2, env)) match {
      case (e@Error(s), _) => e
      case (_, e@Error(s)) => e
      case (IntValue(v1), IntValue(v2)) => BooleanValue(v1 < v2)
      case _ => Error("type error at " + pos + ": required integers")
    }

    case LE(expr1, expr2, pos) => (eval(expr1, env), eval(expr2, env)) match {
      case (e@Error(s), _) => e
      case (_, e@Error(s)) => e
      case (IntValue(v1), IntValue(v2)) => BooleanValue(v1 <= v2)
      case _ => Error("type error at " + pos + ": required integers")
    }

    case Greater(expr1, expr2, pos) => (eval(expr1, env), eval(expr2, env)) match {
      case (e@Error(s), _) => e
      case (_, e@Error(s)) => e
      case (IntValue(v1), IntValue(v2)) => BooleanValue(v1 > v2)
      case _ => Error("type error at " + pos + ": required integers")
    }

    case GE(expr1, expr2, pos) => (eval(expr1, env), eval(expr2, env)) match {
      case (e@Error(s), _) => e
      case (_, e@Error(s)) => e
      case (IntValue(v1), IntValue(v2)) => BooleanValue(v1 >= v2)
      case _ => Error("type error at " + pos + ": required integers")
    }
    case Equal(expr1, expr2, pos) => (eval(expr1, env), eval(expr2, env)) match {
      case (e@Error(s), _) => e
      case (_, e@Error(s)) => e
      case (IntValue(v1), IntValue(v2)) => BooleanValue(v1 == v2)
      case (StringValue(v1), StringValue(v2)) => BooleanValue(v1 == v2)
      case (BooleanValue(v1), BooleanValue(v2)) => BooleanValue(v1 == v2)
      case _ => Error("type error at " + pos + ": operands must be same type")
    }

    case NotEqual(expr1, expr2, pos) => (eval(expr1, env), eval(expr2, env)) match {
      case (e@Error(s), _) => e
      case (_, e@Error(s)) => e
      case (IntValue(v1), IntValue(v2)) => BooleanValue(v1 != v2)
      case (StringValue(v1), StringValue(v2)) => BooleanValue(v1 != v2)
      case (BooleanValue(v1), BooleanValue(v2)) => BooleanValue(v1 != v2)
      case _ => Error("type error at " + pos + ": required integers")
    }

    case Add(expr1, expr2, pos) => (eval(expr1, env), eval(expr2, env)) match {
      case (e@Error(s), _) => e
      case (_, e@Error(s)) => e
      case (IntValue(v1), IntValue(v2)) => IntValue(v1 + v2)
      case (StringValue(v1), v2) => StringValue(v1 + v2.toString)
      case (v1, StringValue(v2)) => StringValue(v1.toString + v2)
      case _ => Error("type error at " + pos + ": required strings or integers")

    }

    case Sub(expr1, expr2, pos) => (eval(expr1, env), eval(expr2, env)) match {
      case (e@Error(s), _) => e
      case (_, e@Error(s)) => e
      case (IntValue(v1), IntValue(v2)) => IntValue(v1 - v2)
      case _ => Error("type error at " + pos + ": required integers")
    }

    case Neg(expr1, pos) => eval(expr1, env) match {
      case e@Error(s) => e
      case IntValue(v) => IntValue(-v)
      case _ => Error("type error at " + pos + ": required integer")
    }


    case Mult(expr1, expr2, pos) => (eval(expr1, env), eval(expr2, env)) match {
      case (e@Error(s), _) => e
      case (_, e@Error(s)) => e
      case (IntValue(v1), IntValue(v2)) => IntValue(v1 * v2)
      case _ => Error("type error at " + pos + ": required integers")
    }

    case Div(expr1, expr2, pos) => (eval(expr1, env), eval(expr2, env)) match {
      case (e@Error(s), _) => e
      case (_, e@Error(s)) => e
      case (_, IntValue(0)) => Error("runtime error at " + pos + " division by zero")
      case (IntValue(v1), IntValue(v2)) => IntValue(v1 / v2)
      case _ => Error("type error at " + pos + ": required integers")
    }

    case Var(name, None, pos) => env.get(name) match {
      case value: Value => value
      case _ => Error("runtime error at " + pos + ": undefined variable " + name)
    }

    case Var(name, Some(indexExpr), pos) => env.get(name) match {
      case ArrayValue(array, _) => eval(indexExpr, env) match {
        case IntValue(index) =>
          if (index > 0 && index <= array.size)
            array(index - 1)
          else
            Error("runtime error at " + pos + ": out of bound")
        case e@Error(_) => e
        case _ => Error("type error")
      }
      case _ => Error("runtime error at " + pos + ": undefined variable " + name)
    }

    case NumberLiteral(value) => IntValue(value)
    case StringLiteral(value) => StringValue(value)
    case BooleanLiteral(value) => BooleanValue(value)
  }

}

