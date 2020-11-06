package stmt

import stmt.TYPE.TYPE
//不完整，需要后续的修改
class Stmt_call extends Stmt {
  override protected val t: TYPE = TYPE.Call
  private var length: Int = 0
  private var ret: Int = -1
  private var args: Array[Int] = null

  def this(str: String) = {
    this()
    val array = str.split('\t')
    var set_string = Set.empty[String]
    if(array(0).charAt(0) == 'a'){

    }
    else if(array(0).charAt(0) == 'r'){

    }
    else{
      println("wrong arg type!!!")
    }

    this.length = set_string.size
    this.args = new Array[Int](this.length)

  }
  def getLength(): Int = length
  def getRet(): Int = ret
  def getArgs(): Array[Int] = args
  override def toString_sub(): String = "test"
}
