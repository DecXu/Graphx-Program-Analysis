package stmt
import stmt.TYPE.TYPE

class Stmt_ret extends Stmt{
  override protected val t: TYPE = TYPE.Ret

  override def toString_sub(): String = "ret"
}
