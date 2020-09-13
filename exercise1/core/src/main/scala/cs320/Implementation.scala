package cs320

object Implementation extends Template with Assignment {

  def volumeOfCuboid(a: Int, b: Int, c: Int): Int = a*b*c

  def concat(x: String, y: String): String = x+y

  def addN(n: Int): Int => Int = {
    def addn(b:Int):Int = {
      n+b
    }
    addn
  }

  def twice(f: Int => Int): Int => Int = {
    def result(n:Int):Int = {
      f(n)+f(n)-n
    }
    result
  }

  def compose(f: Int => Int, g: Int => Int): Int => Int = {
    def composed(num:Int):Int = {
      f(g(num))
    }  
    composed
  }

  def double(l: List[Int]): List[Int] = {
    def rec(list:List[Int]):List[Int] = {
      list match{
        case List() => List()
        case num1::tail_list => (2*num1)::rec(tail_list)
      }
    }
    rec(l)
  }

  def sum(l: List[Int]): Int = {
    def rec(list:List[Int]):Int = {
      list match{
        case List() => 0 
        case num::tail => num+rec(tail)
      }
    }
    rec(l)
  }

  def getKey(m: Map[String, Int], s: String): Int = {
    m.get(s) match {
      case None => error(s)
      case Some(num) => num 
    }
  }

  def countLeaves(t: Tree): Int = {
    def count(tree: Tree):Int = {
      tree match{
        case Leaf(num) => 1
        case Branch(left, value, right) => count(left)+count(right)
      }
    }
    count(t)
  }

  def flatten(t: Tree): List[Int] = {
    def rec(tree:Tree):List[Int] = {
      tree match {
        case Leaf(value)=>List(value)
        case Branch(left,value,right) => rec(left)++List(value)++rec(right)
      }
    }
    rec(t)
  }
}
