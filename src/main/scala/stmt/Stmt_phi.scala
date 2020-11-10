package stmt

import java.util.Scanner

import org.apache.spark.graphx.VertexId
import stmt.TYPE.TYPE

import scala.collection.mutable

class Stmt_phi extends Stmt {

  override protected val t: TYPE = TYPE.Phi

  private var src: Array[VertexId] = Array.emptyLongArray
  private var dst: VertexId = -1
  private var length: Int = 0

  def this(str: Scanner) = {
    this()

    val dst: String = str.next()
    this.dst = dst.toLong

    val set_string = mutable.Set.empty[String]
    while (str.hasNext()){
      set_string.add(str.next())
    }
    set_string.add("0")

    this.length = set_string.size
    this.src = new Array[VertexId](this.length)
    var i = 0
    for (it <- set_string){
      this.src(i) = it.toLong
      i += 1
    }
  }

  def getLength(): Int = length
  def getDst(): VertexId = dst
  def getSrcs(): Array[VertexId] = src

  override def toString_sub(): String = {
    var str: String = "phi," + getDst() + " <- ["
    for (i <- 0 until length){
      str += src(i)
      str += ", "
    }
    str += "]"
    str
  }
}
