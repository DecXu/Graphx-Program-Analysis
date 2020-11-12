import org.apache.spark.graphx.VertexId
import stmt._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}

object Tools {
  type GraphStore = mutable.HashMap[VertexId, (String,Pegraph)]
  def sum_edge(k: Pegraph): Int = {
    var sum = 0
    for (tmp <- k.getGraph().values){
      sum += tmp.getSize()
    }
    sum
  }

  def update_graphstore(result: GraphStore, msg: GraphStore) = {
    for (key <- msg.keySet) {
      result += (key -> msg(key))
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

    while (!worklist.isEmpty){
      val id = worklist(0)
      worklist = worklist.drop(1)
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
      return new Pegraph
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

    var pre_graph: Pegraph = null
    breakable{
      for (it <- graphstore){
        val pred = it._2
        if (pred._1.contains("call") || pred._1.contains("callfptr")){
          pre_graph = it._2._2
          collect_associated_variables(ids, args, len, ret, pre_graph, grammar)
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

  def getPartial(current_stmt: String, pre_stmt: String, pred_graph: Pegraph, grammar: Grammar, graphstore: GraphStore): Pegraph = {
    var out: Pegraph = null
    if (pre_stmt.contains("callfptr")){
      val callfptrstmt = new CfgNode(pre_stmt).getStmt().asInstanceOf[Stmt_callfptr]
      if (current_stmt.contains("return")){
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
    else if (pre_stmt.contains("call")){
      val callstmt = new CfgNode(pre_stmt).getStmt().asInstanceOf[Stmt_call]
      if (current_stmt.contains("return")){
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
    else if (current_stmt.contains("return")){
      val returnstmt = new CfgNode(current_stmt).getStmt().asInstanceOf[Stmt_return]
      if (returnstmt.getLength() == 0){
        out = new Pegraph()
      }
      else {
        out = extractSubGraph_exit(pred_graph, returnstmt.getArgs(), returnstmt.getLength(), returnstmt.getRet(), grammar, graphstore)
      }
    }
    else out = pred_graph
    out
  }

  def getIn(graphstore: GraphStore, current_stmt: String, grammar: Grammar): Pegraph = {
    var in = new Pegraph
    if (graphstore.size == 1){
      for (value <- graphstore.values){
        val prepegraph = new Pegraph()
        prepegraph.decopy(value._2)
        in = prepegraph
        //in = getPartial(current_stmt, value._1, in, grammar, graphstore)
      }
    }
    else {
      for (value <- graphstore.values){
        val prepegraph = new Pegraph()
        prepegraph.decopy(value._2)
        //in = getPartial(current_stmt, value._1, in, grammar, graphstore)
        in.merge(prepegraph)
      }
    }

    in
  }

}
