package stmt
import stmt.TYPE.TYPE

class Stmt_skip extends Stmt{
  override protected val t: TYPE = TYPE.Skip

  override def toString_sub(): String = "skip"
}
