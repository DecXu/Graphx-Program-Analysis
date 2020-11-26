import java.util.Scanner

import stmt.TYPE.TYPE
import stmt.{Stmt_assign, _}

// 类似于一个工厂方法
class CfgNode {
  private var stmt: Stmt = null

  def this(line: String) = {
    this()
    val s: Scanner = new Scanner(line)
    s.next()
    val stmt_type = s.next()
    //println(stmt_type)
    stmt_type match{
      case "assign" => stmt = new Stmt_assign(s)
      case "load" => stmt = new Stmt_load(s)
      case "store" => stmt = new Stmt_store(s)
      case "alloca" => stmt = new Stmt_alloc(s)
      case "phi" => stmt = new Stmt_phi(s)
      case "call" => stmt = new Stmt_call(s)
      case "return" => stmt = new Stmt_return(s)
      case "ret" => stmt = new Stmt_ret()
      case "block" => stmt = new Stmt_skip()
      case "callfptr" => stmt = new Stmt_callfptr(s)
      case "calleefptr" => stmt = new Stmt_calleefptr(s)
      case _ => {
        println(stmt_type)
        println("wrong stmt type in CfgNode!!!")
        System.exit(1)
      }
    }
  }

  @inline final def getStmt(): Stmt = {
    stmt
  }

}
