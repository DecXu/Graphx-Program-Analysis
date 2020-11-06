import org.apache.spark.graphx.VertexId

import scala.reflect.ClassTag

class EdgeArray2 extends Serializable {

  def this(size: Int, edges: Array[VertexId], labels: Array[Byte]){
    this()
    this.size = size
    this.edges = edges
    this.labels = labels
  }

  private var edges: Array[VertexId] = Array.empty[VertexId]
  private var labels: Array[Byte] = Array.emptyByteArray
  private var size: Int = 0
  private var capacity: Int = 0

  //override def equals(obj: Any): Boolean = super.equals(obj)
  override def equals(obj: Any): Boolean = {
    val other = obj.asInstanceOf[EdgeArray2]
    if (this eq other) return true
    if (this.size != other.size) return false
    for (i <- 0 until this.size){
      if ((this.edges(i) != other.edges(i)) || (this.labels(i) != other.labels(i))) return false
    }
    true
  }

  def decopy(edgearray: EdgeArray2) = {
    val tmp_edges = new Array[VertexId](edgearray.size)
    val tmp_labels = new Array[Byte](edgearray.size)

    val edge = edgearray.getEdges
    val label = edgearray.getLabels
    for (i <- 0 until edgearray.size){
      tmp_edges(i) = edge(i)
      tmp_labels(i) = label(i)
    }
    this.size = edgearray.size
    this.edges = tmp_edges
    this.labels = tmp_labels
  }

  def merge() = {
    // sort edges
    myalgo.quickSort(edges, labels, 0, size - 1)
    //System.out.print(this)
    // remove duplicate edges
    val edges_tmp = new Array[VertexId](size)
    val labels_tmp = new Array[Byte](size)
    var numEdges = 0
    numEdges = myalgo.removeDuple(numEdges, edges_tmp, labels_tmp, size, edges, labels)
    //System.out.print(this)
    size = numEdges
    Array.copy(edges_tmp, 0, edges, 0, numEdges)
    Array.copy(labels_tmp, 0, labels, 0, numEdges)
    //System.out.print(this)
  }

  def getSize(): Int = this.size

  def getEdges() = this.edges

  def getLabels(): Array[Byte] = this.labels

  def isEmpty(): Boolean = (this.size == 0)
  //与C++部分有所不同（不需要处理垃圾回收），之后再进行检查。
  def addOneEdge(edge: VertexId, label: Byte) = {
    if(size == 0){
      //System.out.println("done!")
      capacity = 8
      edges = new Array[VertexId](capacity)
      labels = new Array[Byte](capacity)
      for(i <- 0 until capacity){
        edges(i) = -1
        labels(i) = 127
      }
    }
    else{
      if(size >= capacity){
        capacity *= 2
        edges = myalgo.myrealloc(edges, size, capacity)
        labels = myalgo.myrealloc(labels, size, capacity)
        for(i <- size until capacity){
          edges(i) = -1
          labels(i) = 127
        }
      }
    }
    // add edge
    edges(size) = edge
    labels(size) = label
    size += 1
  }
  //与C++部分有所不同（不需要处理垃圾回收），之后再进行检查。
  def set(size: Int, edges: Array[VertexId], labels: Array[Byte]): Unit = {
    if(size == 0) return
    this.size = size
    this.edges = new Array[VertexId](size)
    this.labels = new Array[Byte](size)
    Array.copy(edges, 0, this.edges, 0, size)
    Array.copy(labels, 0, this.labels, 0, size)
  }

  override def toString: String = {
    var str : String = ""
    str += ("{size=" + size + "; ")
    for(i <- 0 until size){
      str += ("(" + edges(i) + ", " + labels(i) + ")")
    }
    str += '}'
    str
  }
}
