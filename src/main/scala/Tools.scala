import org.apache.spark.graphx.VertexId

import scala.collection.mutable

object Tools {
  def sum_edge(k: Pegraph): Int = {
    var sum = 0
    for (tmp <- k.getGraph().values){
      sum += tmp.getSize()
    }
    sum
  }

  def update_graphstore(result: mutable.Map[VertexId, Pegraph], msg: mutable.HashMap[VertexId, Pegraph]) = {
    for (key <- msg.keySet) {
      result += (key -> msg(key))
    }
  }

  def getIn(in: Pegraph, graphstore: mutable.Map[VertexId, Pegraph]) = {
    for (value <- graphstore.values){
      val prepegraph = new Pegraph()
      prepegraph.decopy(value)
      in.merge(prepegraph)
    }
  }

}
