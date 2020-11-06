class Grammar extends Serializable
{



  def getLabelValue(str: Array[Byte]): Byte = {
    for(i <- 0 until numRawLabels){
      if(rawlabel(i) sameElements str){
        return (i - 128).toByte
      }
    }
    127
  }

  private val rawlabel = Array.ofDim[Byte](256, 36)
  private var numRawLabels = 0

  private val erules = new Array[Byte](256)
  private var numErules = 0
  private val rules = new Array[Byte](65536).map(_ => 127.asInstanceOf[Byte])

  final def addRawLabel(label: Array[Byte]): Int = {
    for(i <- 0 until numRawLabels){
      if(rawlabel(i) sameElements label){ //相等则 return i
        return i
      }
    }
    //如果rawLabel没有
    rawlabel(numRawLabels) = label
    numRawLabels += 1
    numRawLabels - 1
  }

  @inline final def changeShort(a: Byte, b: Byte): Short = {
    (a.toShort << 8 | (b.toShort & 0xFF)).toShort
  }

  @inline final def checkRules(edgeVal: Byte): Byte = rules(changeShort(127, edgeVal) + 32768) // find s-rule edges
  @inline final def checkRules(srcVal: Byte, dstVal: Byte): Byte = rules(changeShort(srcVal, dstVal) + 32768) // find s-rule edges


  def test(): Unit = {
    println("==========GRAMMER TEST START==========")
    println("rawLabels: ")
    for(i <- 0 until numRawLabels){
      print("(");rawlabel(i).foreach(x => print(x.toChar));print(',');print(i - 128);print(')')
    }
    println()
    println("eRules :")
    for(i <- 0 until numErules){
      print(erules(i));print(',')
    }
    println()
    println("s-rules and d-rules: ")
    for(i <- 0 until 65536){
      if(rules(i) != 127){
        val s: Short = (i - 32768).toShort
        val a: Byte = s.toByte //s的低8位
        val b: Byte = (s >> 8).toByte //s的高8位
        //println(java.nio.ByteOrder.nativeOrder)
        if(b == 127) {
          print("s-rule: ");print(rules(i));print(":= ");println(a)
        }
        else {
          print("d-rule: ");print(rules(i));print(":= ");print(b);print(',');println(a)
        }
      }
    }
    println("==========GRAMMAR TEST END============")
  }

  final def loadGrammar(s: String): Unit = {
    val arg = Array.ofDim[Byte](3, 36)
    val index = new Array[Int](3)
    val array = s.split('\t')
    //println('M'.toByte)
    //println(array.length)
    //array.foreach(println)
    for(i <- array.indices){
      arg(i) = array(i).toCharArray.map(x => x.toByte)
      //println(i)
      //arg(i).foreach(x => println(x.toChar))
    }
    //println()
    for(i <- array.indices){
      index(i) = addRawLabel(arg(i))
      //println(index(i))
    }
    array.length match{
      case 1 => {
        erules(numErules) = (index(0) - 128).asInstanceOf[Byte]
        numErules += 1
      }
      case 2 => {
        val tmp: Short = changeShort(127, (index(1) - 128).asInstanceOf[Byte])
        rules(tmp + 32768) = (index(0)-128).asInstanceOf[Byte]
      }
      case 3 => {
        val tmp: Short = changeShort((index(1) - 128).asInstanceOf[Byte], (index(2) - 128).asInstanceOf[Byte])
        rules(tmp + 32768) = (index(0)-128).asInstanceOf[Byte]
      }
      case _ =>
    }
  }

  @inline final def getRawLabel(value: Byte): Array[Byte] = rawlabel(value + 128)

  @inline final def isMemoryAlias(label: Byte): Boolean = {
    val raw:Array[Byte] = this.getRawLabel(label)
    raw sameElements Array('M')
  }

  @inline final def isDereference(label: Byte): Boolean = {
    val raw:Array[Byte] = this.getRawLabel(label)
    raw sameElements Array('d')
  }

  @inline final def isDereference_reverse(label: Byte): Boolean = {
    val raw:Array[Byte] = this.getRawLabel(label)
    raw sameElements Array('-','d')
  }

  @inline final def getNumErules(): Int = numErules

  @inline final def getErule(index: Int): Byte = erules(index)

  @inline final def isDereference_bidirect(label: Byte):Boolean = {
    val raw:Array[Byte] = this.getRawLabel(label)
    (raw sameElements Array('d')) || (raw sameElements Array('-','d'))
  }

  @inline final def isPointsTo(label: Byte): Boolean ={
    val raw:Array[Byte] = this.getRawLabel(label)
    raw sameElements Array('P','t')
  }
  @inline final def isEruleLabel(label: Byte): Boolean = {
    for(i <- 0 until getNumErules()){
      if(label == getErule(i)){
        return true;
      }
    }
    false
  }

}
