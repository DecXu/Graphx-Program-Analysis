package stmt
import java.util.Scanner

import org.apache.spark.graphx.VertexId
import stmt.TYPE.TYPE

class Stmt_assign extends Stmt {
  override protected val t: TYPE = TYPE.Assign
  private var src: VertexId = -1
  private var dst: VertexId = -1

  def this(s: Scanner) = {
    this()
//    val array = str.split('\t')
//    this.src = array(0).toInt
//    this.dst = array(1).toInt
    this.dst = s.next().toInt
    this.src = s.next().toInt
  }
  def getSrc(): VertexId = src
  def getDst(): VertexId = dst

  override def toString_sub(): String = "assign " + ',' + dst + " <- " + src
}
