package cs320

object Implementation extends Template {

  // apply a binary numeric function on all the combinations of numbers from
  // the two input lists, and return the list of all the results
  def binOp(
    op: (Int, Int) => Int,
    ls: List[Int],
    rs: List[Int]
  ): List[Int] = ls match {
    case Nil => List()
    case l :: rest =>
      {
        def f(r: Int): Int = {
        op(l, r)
        }
        rs.map(f) ++ binOp(op, rest, rs)
      }
  }

  def interp(expr: Expr): List[Int] = {
    def Map_find(input_map: Map[String, List[Int]], key:String) = {
      if (input_map.contains(key)) input_map(key)
      else error("free identifier")
    }
    def min(x:Int, y:Int):Int = if (x>y) y else x
    def max(x:Int, y:Int):Int = if (x>y) x else y
    def aux(expr: Expr, env: Map[String, List[Int]]): List[Int] = expr match {
        case Add(l, r) => binOp((x:Int,y:Int)=>x+y, aux(l, env), aux(r, env))
        case Sub(l, r) => binOp((x:Int,y:Int)=>x-y, aux(l, env), aux(r, env))
        case Val(name, expression, body) => aux(body, env + (name -> aux(expression, env)))
        case Id(id) => Map_find(env,id)
        case Num(values) => values
        case Min(left, mid, right) => binOp(min, binOp(min, aux(left,env), aux(mid,env)), aux(right,env))
        case Max(left, mid, right) => binOp(max, binOp(max, aux(left,env), aux(mid,env)), aux(right,env))
      }
      aux(expr, Map())
  }
}
