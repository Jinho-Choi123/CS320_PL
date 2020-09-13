package cs320

object Implementation extends Template {

  def freeIds(expr: Expr): Set[String] = 
  {
    def aux(expr:Expr, bind_list:Set[String]):Set[String] = expr match{
      case Add(l, r) => aux(l, bind_list)++aux(r, bind_list)
      case Sub(l, r) => aux(l, bind_list)++aux(r, bind_list)
      case Val(name, expression, body) => aux(expression, bind_list)++aux(body, bind_list+name)
      case Id(id) => if(bind_list.contains(id)) { Set()} else { Set(id) }
      case Num(_) => Set()
    }
    aux(expr, Set())
  }

  def bindingIds(expr: Expr): Set[String] = {
    def aux(expr:Expr):Set[String] = expr match{
      case Add(l, r) => aux(r)++aux(l)
      case Sub(l, r) => aux(r)++aux(l)
      case Val(name, expression, body) =>aux(expression)++aux(body)+name
      case Id(_) => Set()
      case Num(_) => Set()
    }
    aux(expr)
  }

  def boundIds(expr: Expr): Set[String] = {
    def aux(bind_list:Set[String], expr:Expr):Set[String] = expr match{
      
      case Add(l, r) => aux(bind_list, l)++aux(bind_list,r)
      case Sub(l, r) => aux(bind_list, l)++aux(bind_list,r)
      case Val(name, expression, body) => aux(bind_list, expression)++aux(bind_list+name, body)
      case Id(id) => if(bind_list.contains(id)) {Set(id)} else {Set()}
      case Num(_) => Set()
    }
    aux(Set(), expr)
  }
}
