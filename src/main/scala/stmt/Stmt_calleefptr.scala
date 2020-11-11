package stmt
import java.util.Scanner

import org.apache.spark.graphx.VertexId
import stmt.TYPE.TYPE

class Stmt_calleefptr extends Stmt {
  override protected val t: TYPE = TYPE.Calleefptr

  private var dst: VertexId = -1
  def this(str: Scanner) = {
    this()
    dst = str.next().toLong
  }

  def getDst(): VertexId = dst

  override def toString_sub(): String = "calleefptr, " + getDst()
}
