import org.apache.spark.graphx.VertexId
class MinHeapNode {
  var key_v: VertexId = 0
  var key_c: Byte = 0
  var i: Int = 0 // index of array
  var j: Int = 0 // next element's index
}

class MinHeap{
  def replaceMin(p: MinHeapNode) = {
    harr(0) = p
    MinHeapify(0)
  }

  @inline final def getMin(): MinHeapNode = harr(0)

  private var harr: Array[MinHeapNode] = Array.empty[MinHeapNode]
  private var size: Int = 0;
  def this(a: Array[MinHeapNode], size: Int){
    this()
    this.size = size
    this.harr = a
    var i = (size - 1) / 2
    while(i >= 0){
      MinHeapify(i)
      i -= 1
    }
  }

  @inline final def left(i: Int) = 2 * i + 1
  @inline final def right(i: Int) = 2 * i + 2

  def MinHeapify(i: Int): Unit ={
    val l = left(i)
    val r = right(i)
    var smallest = i
    if(l < size && myalgo.myCompare(harr(l).key_v,harr(l).key_c,harr(i).key_v,harr(i).key_c) < 0){
      smallest = l
    }
    if(r < size && myalgo.myCompare(harr(r).key_v,harr(r).key_c,harr(smallest).key_v,harr(smallest).key_c) < 0){
      smallest = r
    }
    //swap直接写入，避免对参数为val的处理
    if(smallest != i){
      var tmp = harr(i)
      harr(i) = harr(smallest)
      harr(smallest) = tmp
      MinHeapify(smallest)
    }
  }
}