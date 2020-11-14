import org.apache.spark.graphx.VertexId
import stmt._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}

object Tools {
  type GraphStore = mutable.HashMap[VertexId, (String,Pegraph)]
  type Msg = mutable.HashMap[VertexId, (Boolean, String,Pegraph)]
  def sum_edge(k: Pegraph): Int = {
    var sum = 0
    for (tmp <- k.getGraph().values){
      sum += tmp.getSize()
    }
    sum
  }

  def update_graphstore(result: GraphStore, msg: Msg) = {
    for (key <- msg.keySet) {
      //result += (key -> (msg(key)._1, msg(key)._2))
      result += (key -> (msg(key)._2, msg(key)._3))
    }
  }

  def collect_associated_variables(ids: mutable.Set[VertexId], args: Array[VertexId], len: Int, ret: VertexId, graph: Pegraph, grammar: Grammar) = {
    //val worklist = mutable.HashSet.empty[VertexId]
    var worklist = ListBuffer.empty[VertexId]
    //val worklist = mutable.HashSet.empty[VertexId]

    for (i <- 0 until len){
      ids += args(i)
      worklist += args(i)
      //worklist = args(i) :: worklist
    }
    if (ret != -1l){
      ids += ret
      worklist += ret
      //worklist = ret :: worklist
    }

    while (worklist.nonEmpty){
      val id = worklist.head
      worklist -= id
      if (graph.getGraph().contains(id)){
        for (i <- 0 until graph.getNumEdges(id)){
          val dst = graph.getEdges(id)(i)
          if (!ids.contains(dst)){
            ids += dst
            worklist += dst
            //worklist = dst :: worklist
          }
        }
      }
    }
  }

  def extractSubGraph(graph: Pegraph, args: Array[VertexId], len: Int, ret: VertexId, grammar: Grammar): Pegraph = {
    if(len == 0 && ret == -1){
      return new Pegraph()
    }
    val ids = mutable.Set.empty[VertexId]
    collect_associated_variables(ids, args, len, ret, graph, grammar)

    /* keep edges associated with ids */
    for (it <- graph.getGraph()){
      breakable{
        if (it._2.isEmpty()){
          graph.getGraph() -= it._1
          break()
        }
        val src = it._1
        if (!ids.contains(src)){
          graph.getGraph() -= it._1
        }
      }
    }
    graph
  }

  def extractSubGraph_exit(graph: Pegraph, args: Array[VertexId], len: Int, ret: VertexId, grammar: Grammar, graphstore: GraphStore): Pegraph = {
    if(len == 0 && ret == -1){
      return new Pegraph
    }
    val ids = mutable.Set.empty[VertexId]

    var pred_graph: Pegraph = null
    breakable{
      for (it <- graphstore){
        val pred = it._2
        if (pred._1.contains("call") || pred._1.contains("callfptr")){
          pred_graph = it._2._2
          collect_associated_variables(ids, args, len, ret, pred_graph, grammar)
          break()
        }
      }
    }
    val fromExit = new Pegraph
    /* extract edges associated with ids */
    for (it <- ids){
      val id: VertexId = it
      if (graph.getGraph().contains(id)){
        val edges = new EdgeArray2()
        for (i <- 0 until graph.getNumEdges(id)){
          val dst: VertexId = graph.getEdges(id)(i)
          val label: Byte = graph.getLabels(id)(i)
          if (ids.contains(dst)){
            edges.addOneEdge(dst, label)
          }
        }
        if (edges.getSize() != 0) {
          fromExit.setEdgeArray(id, edges)
        }
      }
    }
    fromExit
  }

  def getPartial(current: Stmt, pred: Stmt, pred_graph: Pegraph, grammar: Grammar, graphstore: GraphStore): Pegraph = {
    var out: Pegraph = null
    if (pred.getType() == TYPE.Callfptr){
      val callfptrstmt = pred.asInstanceOf[Stmt_callfptr]
      if (current.getType() == TYPE.Return){
        out = pred_graph
//        if (callfptrstmt.getLength() == 0){
//          out = pred_graph
//        }
//        else {
//          out = pred_graph
//        }
      }
      else {
        out = extractSubGraph(pred_graph, callfptrstmt.getArgs(), callfptrstmt.getLength(), callfptrstmt.getRet(), grammar);
      }
    }
    else if (pred.getType() == TYPE.Call){
      val callstmt = pred.asInstanceOf[Stmt_call]
      if (current.getType() == TYPE.Return){
        out = pred_graph
        //        if (callfptrstmt.getLength() == 0){
        //          out = pred_graph
        //        }
        //        else {
        //          out = pred_graph
        //        }
      }
      else {
        out = extractSubGraph(pred_graph, callstmt.getArgs(), callstmt.getLength(), callstmt.getRet(), grammar);
      }
    }
    else if (current.getType() == TYPE.Return){
      val returnstmt = current.asInstanceOf[Stmt_return]
      if (returnstmt.getLength() == 0){
        out = new Pegraph()
      }
      else {
        println("need extractSubGraph_exit!")
        //out = extractSubGraph_exit(pred_graph, returnstmt.getArgs(), returnstmt.getLength(), returnstmt.getRet(), grammar, graphstore)
      }
    }
    else out = pred_graph
    out
  }

  def getIn(graphstore: GraphStore, current: String, grammar: Grammar): Pegraph = {
    var out = new Pegraph

    if (graphstore.size == 1){
      val value = graphstore.last._2
      out.decopy(value._2)
      out = getPartial(new CfgNode(current).getStmt(), new CfgNode(value._1).getStmt, out, grammar, graphstore)
    }
    else {
      for (value <- graphstore.values){
        var out_graph = new Pegraph
        out_graph.decopy(value._2)
        out_graph = getPartial(new CfgNode(current).getStmt(), new CfgNode(value._1).getStmt, out_graph, grammar, graphstore)
        out.merge(out_graph)
      }
    }
    out
  }

}
