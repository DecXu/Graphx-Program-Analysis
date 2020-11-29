import org.apache.spark.graphx.VertexId

import scala.collection.mutable

class ComputationSet {
  private val Olds =  mutable.Map.empty[VertexId, EdgeArray]
  private val Deltas =  mutable.Map.empty[VertexId, EdgeArray]
  private val News =  mutable.Map.empty[VertexId, EdgeArray]

  @inline final def getOldsNumEdges(index: VertexId): VertexId = Olds(index).getSize
  @inline final def getDeltasNumEdges(index: VertexId): VertexId = Deltas(index).getSize
  @inline final def getNewsNumEdges(index: VertexId): VertexId = News(index).getSize

  @inline final def getOldsEdges(index: VertexId): Array[VertexId] = Olds(index).getEdges()
  @inline final def getOldsLabels(index: VertexId): Array[Byte] = Olds(index).getLabels()

  @inline final def getNewsEdges(index: VertexId): Array[VertexId] = News(index).getEdges()
  @inline final def getNewsLabels(index: VertexId): Array[Byte] = News(index).getLabels()

  def setNews(index: VertexId, numEdges: Int, edges: Array[VertexId], labels: Array[Byte]) = {
    if (!News.contains(index)) {
      News(index) = new EdgeArray()
    }
    News(index).set(numEdges, edges, labels)
  }
  // getters and setters
  @inline final def oldEmpty(index: VertexId) = !Olds.contains(index)

  @inline final def deltaEmpty(index: VertexId) = !Deltas.contains(index)

  @inline final def newEmpty(index: VertexId) = !News.contains(index)

  @inline final def getDeltasLabels(index: VertexId): Array[Byte] = Deltas(index).getLabels()

  @inline final def getDeltasEdges(index: VertexId): Array[VertexId] = Deltas(index).getEdges()

  def getOlds() = Olds

  def getDeltas() = Deltas

  def getNews() = News

  def getVertices(): mutable.Set[VertexId] = {
    val vertexSet = mutable.Set.empty[VertexId]
    for (it <- this.getOlds()) vertexSet.add(it._1)
    for (it <- this.getDeltas()) vertexSet.add(it._1)
    for (it <- this.getNews()) vertexSet.add(it._1)
    vertexSet
  }

  def getDeltasTotalNumEdges(): Long = {
    var num: Long = 0
    for (it <- Deltas) num += getDeltasNumEdges(it._1)
    num
  }

  def setDeltas(index: VertexId, numEdges: Int, edges: Array[VertexId], labels: Array[Byte]) = {
    if (!Deltas.contains(index)) Deltas(index) = new EdgeArray()
    Deltas(index).set(numEdges, edges, labels)
  }

  def setOlds(index: VertexId, numEdges: Int, edges: Array[VertexId], labels: Array[Byte]) = {
    if(!Olds.contains(index)) Olds(index) = new EdgeArray()
    Olds(index).set(numEdges, edges, labels)
  }

  def init_add(out: Pegraph, m: mutable.Map[VertexId, EdgeArray], isConservative: Boolean) = {
    //first get a fixed point over all the old edges
    if (isConservative) {
      // Deltas <- {m, out}
      for (it <- out.getGraph()) {
        val id_old: VertexId = it._1
        if (m.contains(id_old)) {
          val n1 = out.getNumEdges(id_old)
          val n2 = m(id_old).getSize()
          val edges = new Array[VertexId](n1 + n2)
          val labels = new Array[Byte](n1 + n2)

          val len = myalgo.unionTwoArray(edges, labels, n1, out.getEdges(id_old), out.getLabels(id_old),n2,
            m(id_old).getEdges(), m(id_old).getLabels())

          setDeltas(id_old, len, edges, labels)
          m -= id_old
        }
        else {
          setDeltas(it._1, it._2.getSize(), it._2.getEdges(), it._2.getLabels())
        }
      }
      for (it <- m) {
        setDeltas(it._1, it._2.getSize(), it._2.getEdges(), it._2.getLabels())
        //scala 这里可能没有迭代器引起的错误
        m -= it._1
      }
    }
    else {
      // Deltas <- m
      for (it <- m) setDeltas(it._1, it._2.getSize(), it._2.getEdges(), it._2.getLabels())
      m.clear()
      //OldsV <- out
      for(it <- out.getGraph()) setOlds(it._1, it._2.getSize(), it._2.getEdges(), it._2.getLabels())
    }
  }

  def print_graph_map(graph: mutable.Map[VertexId, EdgeArray]): String = {
    var size = 0
    var str = ""
    for (it <- graph) {
      str += (it._1 + " -> " + it._2 + '\n')
      size += it._2.getSize()
    }
    str += ("------------------\n" + "size=" + size + '\n')
    str
  }

  override def toString: String = {
    var str = ""
    str += ("\nComputationSet<<<<\n---------------------\n" + "Olds:\n" + print_graph_map(Olds))
    str += ("Deltas:\n" + print_graph_map(Deltas))
    str += ("News:\n" + print_graph_map(News))
    str += "---------------------\n"
    str
  }

}
