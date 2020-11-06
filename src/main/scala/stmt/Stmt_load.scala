package stmt

import java.util.Scanner

import org.apache.spark.graphx.VertexId
import stmt.TYPE.TYPE

class Stmt_load extends Stmt {
  override protected val t: TYPE = TYPE.Load
  private var src: VertexId = -1
  private var dst: VertexId = -1
  private var auxiliary: VertexId = -1

  def this(s: Scanner) = {
    this()
//    val array = str.split('\t')
//    this.src = array(0).toInt
//    this.dst = array(1).toInt
//    this.auxiliary = array(2).toInt
    this.dst = s.next().toInt
    this.src = s.next().toInt
    this.auxiliary = s.next().toInt
  }
  def getSrc(): VertexId = src
  def getDst(): VertexId = dst
  def getAux(): VertexId = auxiliary
  override def toString_sub(): String = "load " + ',' + dst + " <- " + src + ',' + auxiliary
}
