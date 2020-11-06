import java.util.Scanner

import stmt.TYPE.TYPE
import stmt.{Stmt_assign, _}

class CfgNode {
  private var stmt: Stmt = null

  def this(line: String) = {
    this()
    val s: Scanner = new Scanner(line)
    //val array = line.split('\t')
    s.next()
    val stmt_type = s.next()
    //println(stmt_type)
    stmt_type match{
      case "assign" => {
        stmt = new Stmt_assign(s)
//        println(stmt.asInstanceOf[Stmt_assign].getDst())
//        println(stmt.asInstanceOf[Stmt_assign].getSrc())
      }
      case "load" => {
        stmt = new Stmt_load(s)
      }
      case "store" => {
        stmt = new Stmt_store(s)
      }
      case "alloca" => {
        stmt = new Stmt_alloc(s)
      }
      case _ => {
        println("wrong stmt type!!!")
        System.exit(1)
      }
    }
  }

  @inline final def getStmt(): Stmt = {
    stmt
  }

}
