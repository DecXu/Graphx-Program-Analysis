package stmt
import java.util.Scanner

import org.apache.spark.graphx.VertexId
import stmt.TYPE.TYPE

import scala.collection.mutable

class Stmt_callfptr extends Stmt{
  override protected val t: TYPE = TYPE.Callfptr
  private var dst: VertexId = -1l
  private var auxiliary: VertexId = -1l

  private var length = 0
  private var args: Array[VertexId] = Array.emptyLongArray
  private var ret: VertexId = -1

  def this(str: Scanner) = {
    this()
    val dst: String = str.next()
    val aux: String = str.next()
    this.dst = dst.toLong
    this.auxiliary = aux.toLong

    var arg: String = ""
    val set_string = mutable.Set.empty[String]

    while (str.hasNext()){
      arg = str.next()
      if (arg(0) == 'a') set_string.add(arg.substring(2))
      else if (arg(0) == 'r') this.ret = arg.substring(2).toLong
      else {
        println("wrong arg type!!!")
        System.exit(1)
      }
    }
    this.length = set_string.size
    this.args = new Array[VertexId](this.length)
    var i: Int = 0
    for (it <- set_string){
      this.args(i) = it.toLong
      i += 1
    }
  }

  def getLength(): Int = length
  def getDst(): VertexId = dst
  def getArgs(): Array[VertexId] = args
  def getAux(): VertexId = auxiliary
  def getRet(): VertexId = ret

  override def toString_sub(): String = {
    var str: String = "callfptr," + getDst() + ",  " + getAux() + ", " + getRet() + " <- ["
    for (i <- 0 until length){
      str += args(i)
      str += ", "
    }
    str += "]"
    str
  }
}
