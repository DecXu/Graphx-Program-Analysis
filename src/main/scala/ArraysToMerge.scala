import org.apache.spark.graphx.VertexId
class ArraysToMerge {
  private var edges: Array[VertexId] = Array.empty[VertexId]
  private var labels: Array[Byte] = Array.empty[Byte]
  private var size: Int = 0
  private var capacity: Int = 0

  private var index: Array[VertexId] = Array.empty[VertexId]
  private var addr: Array[VertexId] = Array.empty[VertexId]
  private var arraySize: Int = 0
  private var arrayCapacity: Int = 0

  private var numEdges: Int = 0
  private var resEdges: Array[VertexId] = Array.empty[VertexId]
  private var resLabels: Array[Byte] = Array.empty[Byte]

  override def toString: String = {
    val str = ""
    println("edges: ")
    edges.foreach(println)
    println("labels: ")
    labels.foreach(println)
    println("size: " + size + " capacity: " + capacity)
    println("index: ")
    index.foreach(println)
    println("addr: ")
    addr.foreach(println)
    println("arraySize: " + arraySize + " arrayCapacity: " + arrayCapacity + " numEdges: " + numEdges )
    println("resEdges: ")
    resEdges.foreach(println)
    println("resLabels: ")
    resLabels.foreach(println)
    str
  }

  def getLabelsFirstAddr() = resLabels

  def getEdgesFirstAddr() = resEdges

  def getNumEdges(): Int = numEdges

  def addOneEdge(edge: VertexId, label: Byte) = {
    if (arraySize != 0) {
      if (size >= capacity) {
        capacity *= 2
        edges = myalgo.myrealloc(edges,size,capacity)
        labels = myalgo.myrealloc(labels,size,capacity)
        for (i <- size until capacity) {
          edges(i) = -1
          labels(i) = 127
        }
      }
      // add edge
      if (index(arraySize - 1) == 0) {
        addr(arraySize - 1) = size
      }
      edges(size) = edge
      labels(size) = label
      index(arraySize - 1) += 1
      size += 1
    }
    else {
      print("add edge failed! ")
    }
  }

  def addOneContainer() = {
    if (arraySize == 0) {
      arrayCapacity = 8
      capacity = 8
      edges = new Array[VertexId](capacity)
      labels = new Array[Byte](capacity)
      index = new Array[VertexId](arrayCapacity)
      addr = new Array[VertexId](arrayCapacity)
      var i = 0
      while (i < capacity) {
        edges(i) = -1
        labels(i) = 127
        i += 1
      }
      for (i <- 0 until arrayCapacity) {
        index(i) = 0
        addr(i) = 0
      }
    }
    else {
      if (arraySize >= arrayCapacity) {
        arrayCapacity *= 2
        index = myalgo.myrealloc(index, arraySize, arrayCapacity)
        addr = myalgo.myrealloc(addr,arraySize,arrayCapacity)
        for (i <- arraySize until arrayCapacity) {
          index(i) = 0
          addr(i) = 0
        }
      }
    }
    // add one empty array
    arraySize += 1
  }

  def mergeKArrays() = {
    if (size != 0) {
      var newEdges: Array[VertexId] = Array.empty[VertexId]
      var newLabels: Array[Byte] = Array.empty[Byte]
      // minHeap algorithm to merge k arrays
      if (arraySize > 1) {
        newEdges = new Array[VertexId](size)
        newLabels = new Array[Byte](size)
        // initial k-MinHeap
        val harr: Array[MinHeapNode] = new Array[MinHeapNode](arraySize)
        for (i <- 0 until arraySize) harr(i) = new MinHeapNode

        for (i <- 0 until arraySize) {
          harr(i).key_v = edges(addr(i).toInt)
          harr(i).key_c = labels(addr(i).toInt)
          harr(i).i = i
          harr(i).j = 1
        }
        val hp: MinHeap = new MinHeap(harr, arraySize)
        for (i <- 0 until arraySize) {
          for (j <- 0 until index(i).toInt) {
            val root: MinHeapNode = hp.getMin()
            newEdges((addr(i) + j).toInt) = root.key_v
            newLabels((addr(i) + j).toInt) = root.key_c
            if(root.j < index(root.i)) {
              root.key_v = edges((addr(root.i) + root.j).toInt)
              root.key_c = labels((addr(root.i) + root.j).toInt)
              root.j += 1
            }
            else {
              root.key_v = Int.MaxValue
            }
            hp.replaceMin(root)
          }
        }
      }
      // remove duplicate edges
      val edge_v: Array[VertexId]  = new Array[VertexId](size)
      val edge_l: Array[Byte] = new Array[Byte](size)
      var len: Int = 0
      if (arraySize > 1) {
        len = myalgo.removeDuple(len,edge_v,edge_l,size,newEdges,newLabels)
      }
      else {
        len = myalgo.removeDuple(len,edge_v,edge_l,size,edges,labels)
      }
      numEdges = len
      Array.copy(edge_v, 0 , edges, 0, len)
      Array.copy(edge_l, 0 , labels, 0, len)
      resEdges = edges
      resLabels = labels
    }
  }

  def merge() = mergeKArrays()

}
