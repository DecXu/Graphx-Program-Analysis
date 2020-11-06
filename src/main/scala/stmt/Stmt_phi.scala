package stmt

import stmt.TYPE.TYPE

class Stmt_phi extends Stmt {
  override protected val t: TYPE = TYPE.Phi
  private var src: Int = -1
  private var dst: Int = -1
  private var length: Int = 0
  override def toString_sub(): String = "test"
}
