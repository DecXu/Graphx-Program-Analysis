import org.apache.spark.graphx.VertexId

import scala.collection.mutable

class Pegraph extends Serializable {

  private val graph =  mutable.Map.empty[VertexId, EdgeArray2]

  def this(vertexId: VertexId){
    this()
    graph.put(vertexId, new EdgeArray2)
  }

  override def equals(obj: Any): Boolean = {
    val pegraph = obj.asInstanceOf[Pegraph]
    if (graph.contains(-1l)) return false
    if (this eq pegraph) return true
    if (graph.size != pegraph.graph.size) return false
    for (key <- graph.keySet){
      if (!pegraph.graph.contains(key)) return false
      else if (!graph(key).equals(pegraph.graph(key))) return false
    }
    true
  }

  def merge(prepegraph: Pegraph) = {
    val prepegraph_graph = prepegraph.getGraph()
    //val graph = in.getGraph()
    for (key <- prepegraph_graph.keySet){
      if (!graph.contains(key)){
        graph += (key -> prepegraph_graph(key))
      }
      else {
        val n1 = prepegraph_graph(key).getSize
        val n2 = this.getNumEdges(key)
        val edges_tmp = new Array[VertexId](n1 + n2)
        val labels_tmp = new Array[Byte](n1 + n2)
        val size = myalgo.unionTwoArray(edges_tmp, labels_tmp, n1, prepegraph_graph(key).getEdges(), prepegraph_graph(key).getLabels(), n2, this.getEdges(key), this.getLabels(key))
        graph(key).set(size, edges_tmp, labels_tmp)
      }
    }
  }

  def getEdges(vertexId: VertexId ) = graph(vertexId).getEdges()

  def getLabels(vertexId: VertexId ) = graph(vertexId).getLabels()

  def decopy(value: Pegraph) = {
    for (key <- value.graph.keySet){
      val tmp = new EdgeArray2
      tmp.decopy(value.graph(key))
      graph.put(key, tmp)
    }
  }

  def getGraph(): mutable.Map[VertexId, EdgeArray2] = graph

  def getNumEdges(index: VertexId): Int = {
    if(!graph.contains(index)) graph += (index -> new EdgeArray2())
    graph(index).getSize()
  }

  def setEdgeArray(index: VertexId, numEdges: Int, edges: Array[VertexId], labels: Array[Byte]): Unit = {
    if(!this.graph.contains(index)){
      this.graph.put(index, new EdgeArray2())
    }
    this.graph(index).set(numEdges, edges, labels)
  }

  def setEdgeArray(index: VertexId, array: EdgeArray2): Unit = this.graph(index) = array


  def print_graph_map(): String = {
    var str: String = ""
    var size: Int = 0
    for(it <- graph){
      str += it._1
      str += " -> "
      str += it._2
      str += '\n'
      size += it._2.getSize()
    }
    str += "------------------\nsize="
    str += size
    str += '\n'
    str
  }

  override def toString: String = "PEGraph<<<<\n---------------------\n" + print_graph_map() + "---------------------\n"
}
