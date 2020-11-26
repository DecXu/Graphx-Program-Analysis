package stmt

import java.util.Scanner

import org.apache.spark.graphx.VertexId
import stmt.TYPE.TYPE

import scala.collection.mutable
//不完整，需要后续的修改
class Stmt_call extends Stmt {
  override protected val t: TYPE = TYPE.Call
  private var length: Int = 0
  private var ret: VertexId = -1l
  private var args: Array[VertexId] = Array.emptyLongArray

  def this(str: Scanner) = {
    this()

    var arg: String = ""
    val set_string = mutable.Set.empty[String]
    while (str.hasNext()){
      arg = str.next()
      if (arg(0) == 'a') set_string.add(arg.substring(2))
      else if (arg(0) == 'r') this.ret = arg.substring(2).toLong
      else {
        println("wrong arg type in call!!!")
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
  def getRet(): VertexId = ret
  def getArgs(): Array[VertexId] = args

  override def toString_sub(): String = {
    var str: String = "call, " + getRet() + " <- ["
    for (i <- 0 until length){
      str += args(i)
      str += ", "
    }
    str += "]"
    str
  }
}
