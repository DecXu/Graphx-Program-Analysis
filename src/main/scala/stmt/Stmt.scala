package stmt
object TYPE extends Enumeration{
  type TYPE = Value
  val Assign,
  Load,
  Store,
  Alloca,
  Phi,
  Call,
  Return,
  Ret,
  Skip,
  Callfptr,
  Calleefptr = Value
}

import TYPE._

abstract class Stmt {
  protected val t: TYPE

  def getType(): TYPE = t

  def toString_sub(): String

  override def toString: String = "(" + toString_sub() + ")"
}
