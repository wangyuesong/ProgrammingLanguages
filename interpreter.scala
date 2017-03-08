import scala.io._
import cs162.assign5.syntax._
import Aliases._
import scala.io.Source.fromFile

//—————————————————————————————————————————————————————————————————————————
// Main entry point

object Interpreter {
  type Environment = scala.collection.immutable.HashMap[Var, Value]

  object StuckException extends Exception

  def main(args: Array[String]) {
    val filename = args(0)
    val input = fromFile(filename).mkString
    //    var replicated_state: State = 
    Parsers.program.run(input, filename) match {
      // parsing error: program is not well-formed
      case Left(e) => println(e)
      // successfully parsed: program is well-formed
      case Right(program) => try {
        var curr_state = State(program.e, new Environment(), Seq())
        while (!curr_state.fin) {
//          print("Term: ")
//          println(curr_state.t)
//          println("Env:")
//          curr_state.env.foreach(kv => println("KEY=> " + kv._1 + "\n" + "VALUE=> " + kv._2))
//          println()
//          println("Konts:")
//          curr_state.ks.foreach { x => println(x) }
          //          println(curr_state.ks)
//          println("----------------------------")
          curr_state = curr_state.next

        }
        curr_state.t match {
          case v: Value ⇒
            println(s"Result: ${PrettyValue.prettyVal(v)}")
          case e: Exp ⇒
            // Should never happen
            println(s"Error: unevaluated expression resulted: ${Pretty.prettyExp(e)}")
        }
      } catch {
        case StuckException ⇒ println("Program is stuck")
      }
    }
  }
}

import Interpreter._

case class State(t: Term, env: Environment, ks: Seq[Kont]) {

  def fin: Boolean = {
    t match {
      case e: Exp   => false
      case v: Value => if (ks.isEmpty) true else false
    }
  }

  def next: State = t match {
    case e: Exp => {
      e match {
        // variables
        case x: Var => {
          State(env.getOrElse(x, throw StuckException), env, ks)
        }

        // numeric literals
        case Num(n) => {
          State(NumV(n), env, ks)
        }

        // boolean literals
        case Bool(b) => {
          State(BoolV(b), env, ks)
        }

        // `nil` - the literal for unit
        case _: NilExp => {
          State(NilV, env, ks)
        }

        // builtin arithmetic operators
        case Plus | Minus | Times | Divide | LT | EQ | And | Or | Not => throw StuckException // You shouldn't change this.

        // function creation
        // Closure will record current env.
        case Fun(params, body) => {
//          println("Rule 7: Func Def")
          State(ClosureV(params.map(p => p._1), body, env), env, ks)
        }

        // function call
        case Call(fun, args) => {
//          println("Rule 8: Call")
          fun match {
            case Not => {
              State(args(0), env, (NotK +: ks))
            }
            case op: Builtin => {
              State(args(0), env, (BinopLeftK(op, args(1)) +: ks))
            }
            //           FIX ME
            // Do we need to check if fun is of expression type is a closure?
            case _ => State(fun, env, (AppK(args, List[Value]()) +: ks))
          }
        }

        // conditionals 
        case If(e1, e2, e3) => State(e1, env, IfK(e2, e3) +: ks)

        // let binding
        case Let(x, e1, e2) => {
//          println("Rule 10: Let")
          State(e1, env, LetK(x, e2) +: ks)
        }

        // recursive binding
        case Rec(x, _, e1, e2) => State(e2, env + (x -> LetRecV(x, e1, env)), RestoreK(env) +: ks)

        // record literals
        case Record(fields) => State(fields.map(_._2).head, env,
          RecordK(fields.map(_._1).toList, fields.map(_._2).tail.toList, List[Value]()) +: ks)

        // record access
        case Access(e, field)          => State(e, env, AccessK(field) +: ks)

        // constructor use
        case Construct(constructor, e) => State(e, env, ConsK(constructor) +: ks)

        // pattern matching (case ... of ...)
        case Match(e, cases)           => State(e, env, CaseK(cases) +: ks)
      }
    }

    case v: Value => {
      v match {
        // What is this?
        case LetRecV(x, e1, env1) =>
          State(e1, env1 + (x -> LetRecV(x, e1, env1)), RestoreK(env) +: ks)
        case _ => ks.head match {
          // restore continuation
          case RestoreK(env1)      => State(v, env1, ks.tail)

          // binary operation continuations
          case BinopLeftK(op, e)   => State(e, env, BinopRightK(op, v) +: ks.tail)

          case BinopRightK(op, v1) => State(valueOf(op, v1, v), env, ks.tail)

          // not operation continuation
          case NotK => State(v match {
            case BoolV(true)  => BoolV(false)
            case BoolV(false) => BoolV(true)
            case _            => throw StuckException
          }, env, ks.tail)

          // function application continuation
          case AppK(argsE, vals) => {
//            println("Kont Rule 23 Or 24: AppK")
            argsE.length match {
              case 0 => {
                val rVals = (v +: vals).reverse.toList
                // Get the closure created by rule 7
                val vClosure = rVals.head match {
                  case v: ClosureV => v
                  case _           => throw StuckException
                }
                // Execute the closure body with para->val mapping. 
                // Add RestoreK to top of cont stack. When closure body evaluated 
                // finished to a value. Will see this RestoreK and drop env', using previous
                // stored env and closure body evluated result on Term position
                val vParams = rVals.tail
                val params = vClosure.params
                val l1 = vParams.length
                val l2 = params.length
                val zipped = vClosure.params.zip(vParams);
                State(vClosure.body, vClosure.env ++ vClosure.params.zip(vParams).toMap, RestoreK(env) +: ks.tail)
              }
              case _ => State(argsE.head, env, AppK(argsE.tail, v +: vals) +: ks.tail)
            }
          }
          // if continuation
          case IfK(e2, e3) => {
            v match {
              case BoolV(true)  => State(e2, env, ks.tail)
              case BoolV(false) => State(e3, env, ks.tail)
              case _            => throw StuckException
            }
          }

          // let continuation
          case LetK(x, e2) => {
//            println("Kont Rule 27: Let")
            State(e2, env + (x -> v), RestoreK(env) +: ks.tail)
          }

          // record continuation
          case RecordK(fields, es, vals) => es.length match {
            case 0 => State(RecordV(fields.zip((v +: vals).reverse).toMap), env, ks.tail)
            case _ => State(es.head, env, RecordK(fields, es.tail, v +: vals) +: ks.tail)
          }

          // access continuation
          case AccessK(field) => {
            v match {
              case RecordV(fvMap) => State(fvMap.getOrElse(field, throw StuckException), env, ks.tail)
              case _              => throw StuckException
            }
          }

          // constructing user-defined data type continuation
          case ConsK(constructor) => State(ConstructorV(constructor, v), env, ks.tail)

          // case continuation
          case CaseK(cases) => {
            v match {
              case ConstructorV(constructor, v) => {
                val c = cases.find(_._1.equals(constructor)).getOrElse(throw StuckException)
                State(c._3, env + (c._2 -> v), RestoreK(env) +: ks.tail)
              }
              case _ => throw StuckException
            }
          }
        }
      }
    }
  }

  def valueOf(op: Builtin, v1: Value, v2: Value): Value = {
    v1 match {
      case NumV(i1) => v2 match {
        case NumV(i2) => op match {
          case Plus   => NumV(i1 + i2)
          case Minus  => NumV(i1 - i2)
          case Times  => NumV(i1 * i2)
          case Divide => NumV(i1 / i2)
          case LT     => BoolV(i1 < i2)
          case EQ     => BoolV(i1 == i2)
          case _      => throw StuckException
        }
        case _ => throw StuckException
      }

      case BoolV(b1) => v2 match {
        case BoolV(b2) => op match {
          case And => BoolV(b1 & b2)
          case Or  => BoolV(b1 || b2)
          case _   => throw StuckException
        }
        case _ => throw StuckException
      }
      case _ => throw StuckException
    }
  }
}

sealed abstract class Value extends Term
case class NumV(n: BigInt) extends Value
case class BoolV(b: Boolean) extends Value
case object NilV extends Value
case class ClosureV(params: Seq[Var], body: Exp, env: Environment) extends Value
case class RecordV(fields: Map[Label, Value]) extends Value
case class ConstructorV(constructor: Label, v: Value) extends Value
case class LetRecV(x: Var, e: Exp, env: Environment) extends Value

sealed abstract class Kont
case object NotK extends Kont
case class BinopLeftK(op: Builtin, e: Exp) extends Kont
case class BinopRightK(op: Builtin, v: Value) extends Kont
case class AppK(argsE: Seq[Exp], vals: Seq[Value]) extends Kont
case class IfK(e2: Exp, e3: Exp) extends Kont
case class LetK(x: Var, e2: Exp) extends Kont
case class RecordK(fields: Seq[Label], es: Seq[Exp], vals: Seq[Value]) extends Kont
case class AccessK(field: Label) extends Kont
case class ConsK(constructor: Label) extends Kont
case class CaseK(cases: Seq[(Label, Var, Exp)]) extends Kont
case class RestoreK(env: Environment) extends Kont

object PrettyValue {
  def prettyVal(v: Value): String = {
    def prettyEnv(env: Environment) = env.map({ case (x, v) ⇒ s"${Pretty.prettyExp(x)} ↦ ${prettyVal(v)}" }).mkString(", ")

    v match {
      case NumV(n)  ⇒ n.toString
      case BoolV(b) ⇒ b.toString
      case NilV     ⇒ "()"
      case ClosureV(params, body, env) ⇒
        val pparams = params.map(Pretty.prettyExp).mkString(", ")
        s"Closure: ((${pparams}) ⇒ ${Pretty.prettyExp(body)}), ρ = { ${prettyEnv(env)} }"
      case RecordV(fields) ⇒
        val pfields = fields.map { case (f, v) ⇒ s"$f = ${prettyVal(v)}" }
        s"[ ${pfields.mkString(", ")} ]"
      case ConstructorV(constructor, v) ⇒
        s"$constructor ${prettyVal(v)}"
      case LetRecV(x, e, env) ⇒
        s"LetRec: ${Pretty.prettyExp(x)} = ${Pretty.prettyExp(e)}, ρ = { ${prettyEnv(env)} }"
    }
  }
}
