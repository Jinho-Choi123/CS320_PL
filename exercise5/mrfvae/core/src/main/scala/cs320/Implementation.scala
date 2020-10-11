package cs320

import Value._

object Implementation extends Template {

  def interp(expr: Expr): Value = {
    def interpret(expr:Expr, env:Env):Value = {
      expr match {
        case Num(n) => NumV(n)

        case Add(l, r) => (interpret(l, env), interpret(r, env)) match{
          case (NumV(left), NumV(right)) => NumV(left+right)
          case _ => error("Type mismatch")
        }

        case Sub(l, r) => (interpret(l, env), interpret(r, env)) match{
          case (NumV(left), NumV(right)) => NumV(left-right)
          case _ => error("Type mismatch")
        }

        case Val(name, v, b) => {
          val nenv = env + (name -> interpret(v, env))
          interpret(b, nenv)
        }

        case Id(name) => {
          env.get(name) match {
            case Some(n) => n
            case None => error("Free Identifier")
          }
        }

        case Fun(params, body) => {
          CloV(params, body, env)
        }

        case App(func, args) => {
          interpret(func, env) match {
            case CloV(params, body, fenv) => {
              def iter(params:List[String], args:List[Expr], nenv:Env):Env = {
                (params, args) match {
                  case (List(), List()) => nenv
                  case (h1::t1, h2::t2) => iter(t1, t2, nenv + (h1 -> interpret(h2, env))) 
                  case (_, _) => error("wrong arity")
                }
              }
              val nenv = iter(params, args, fenv)
              interpret(body, nenv)
            }
            case _ => error("not a closure")
          }
        }

        case Rec(rec) => {
          RecV(rec.map{ case (str, e) => (str, interpret(e, env))})
        }

        case Acc(e, name) => {
          val rec = interpret(e, env)
          rec match {
            case RecV(m) => {
              (m.get(name)) match {
                case None => error("no such field")
                case Some(n) => n 
              }
            }
            case _ => error("not a record")
          }
        }

      }
    }
    interpret(expr, Map[String, Value]())
  }

}
