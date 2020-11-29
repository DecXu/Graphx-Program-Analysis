import org.apache.log4j.{Level, Logger}
import org.apache.spark.graphx.PartitionStrategy.RandomVertexCut
import org.apache.spark.{SparkConf, SparkContext, SparkFiles}
import org.apache.spark.graphx.{VertexId, _}
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel
import stmt.{Stmt, Stmt_calleefptr, Stmt_callfptr, Stmt_return, TYPE}

import scala.io.Source
import collection.mutable.ArrayBuffer
import scala.collection.mutable

object Analysis
{
  def main(args: Array[String]): Unit =
  {
    //记录运行时间
    //屏蔽日志
    Logger.getLogger("org.apache.spark").setLevel(Level.WARN)
    Logger.getLogger("org.eclipse.jetty.server").setLevel(Level.OFF)

    val startTime1 = System.currentTimeMillis

    type GraphStore = mutable.HashMap[VertexId, (String, Pegraph)]
    // Boolean代表getin和updategraphstore的标志信息,C++系统中存在只更新本节点但不唤醒后继节点的情况，要进行特殊考虑（也是此标志位存在的意义）
    type Msg = mutable.HashMap[VertexId, (Boolean, String, Pegraph)]
    /**
     * @param stmt : 节点所代表的语句信息
     * @param changed : 表达节点在transfer计算完成后，pegraph信息是否发生变化
     * @param pegraph : 节点pegraph的当前信息
     * @param graphstore : 节点的in集合，保存前置节点的类型信息和pegraph信息
     */
    case class VertexValue(stmt: String, changed: Boolean, pegraph: Pegraph, graphstore: GraphStore)

    //开启Kryo的压缩
    val conf = new SparkConf()
    //graphx.GraphXUtils.registerKryoClasses(conf)
    conf.setAppName("Analysis")
    //  .set("spark.serializer", "org.apache.spark.serializer.KryoSerializer")
        .setMaster("local[2]")
    //  .set("spark.kryo.registrationRequired", "true")
    //  .registerKryoClasses(Array(classOf[VertexValue]
    //    ,classOf[java.util.HashMap[java.lang.Long, java.util.HashMap[java.lang.Long, EdgeArray]]]
    //    ,classOf[Array[Edge[_]]]))
    //    ,classOf[org.apache.spark.graphx.Edge$mcI$sp]]))
    val sc = new SparkContext(conf)

    //VertexValue： VertexValue是节点属性的类型
    val vertexArr = new ArrayBuffer[(VertexId, VertexValue)]()
    //Edge[Byte]: Byte是边属性的类型
    val edgeArr = new ArrayBuffer[Edge[Byte]]()

//**************************HDFS*************************************
  /*  val file_start = "hdfs://slave201:9000/analysis/start"
    sc.addFile(file_start)
    val path_start = SparkFiles.get("start")
    val source_start = Source.fromFile(path_start)
    val lines_start = source_start.getLines()

    val start: Array[String] = new Array[String](4)
    var i = 0
    while(lines_start.hasNext){
      val en = lines_start.next()
      start(i) = en
      i = i + 1
    }
    val file_entry = start(0)
    val file_final = start(1)
    val file_stmt = start(2)
    val file_singleton = start(3)

    sc.addFile(file_entry)
    sc.addFile(file_final)
    sc.addFile(file_stmt)
    sc.addFile(file_singleton)

    val path_entry = SparkFiles.get("entry")
    val path_final = SparkFiles.get("final")
    val path_stmt = SparkFiles.get("id_stmt_info")
    val path_singleton = SparkFiles.get("singleton")*/
    //println(path)
    //val source_entry = Source.fromFile(path_entry)
    //val lineIterator = source.getLines
    //val lines =lineIterator.toArray
    //println(lines.mkString(","))
//**************************HDFS*************************************

    val source_entry =  Source.fromFile("/home/decxu/Documents/analysis_data/test/entry.txt","UTF-8")
    //val source_entry =  Source.fromFile(path_entry)
    val lines_entry = source_entry.getLines()
    //entry包含所有cfg入口点
    val entry = mutable.Set[Long]()
    while(lines_entry.hasNext)
    {
      val en = lines_entry.next().toLong
      entry.add(en)
    }
    //entries是entry的广播变量
    val entries = sc.broadcast(entry)

    val sourceE =  Source.fromFile("/home/decxu/Documents/analysis_data/test/final","UTF-8")
    //val sourceE =  Source.fromFile(path_final)
    val linesE = sourceE.getLines()
    while(linesE.hasNext)
    {
      val ee = linesE.next().split("\t")
      edgeArr += Edge(ee(0).toLong, ee(1).toLong, 0)
    }

    //val sourceV =  Source.fromFile(path_stmt)
    val sourceV =  Source.fromFile("/home/decxu/Documents/analysis_data/test/id_stmt_info.txt","UTF-8")
    val linesV = sourceV.getLines
    while(linesV.hasNext)
    {
      val stmt = linesV.next()
      val id = stmt.split("\t")(0).toLong
      //-2l表示初始状态
      vertexArr += ((id, VertexValue(stmt, false, new Pegraph(-1l), new GraphStore())))
    }

    //val sourceSingleton =  Source.fromFile(path_singleton)
    val sourceSingleton =  Source.fromFile("/home/decxu/Documents/analysis_data/test/var_singleton_info.txt","UTF-8")
    val linesSingleton = sourceSingleton.getLines()
    var SingletonBuffer = new ArrayBuffer[VertexId]()
    while(linesSingleton.hasNext) {
      val s = linesSingleton.next()
      SingletonBuffer += s.toLong
    }
    val singleton = sc.broadcast(new Singleton(SingletonBuffer.toSet))

    val sourceG = Source.fromFile("/home/decxu/Documents/analysis_data/simple/rules_pointsto.txt","UTF-8")
    val linesG = sourceG.getLines()
    val grammars = new Grammar()
    while(linesG.hasNext){
      val line = linesG.next()
      grammars.loadGrammar(line)
    }
    grammars.test()
    val grammar = sc.broadcast(grammars)

    val vertices: RDD[(VertexId, VertexValue)] = sc.parallelize(vertexArr)
    val edges: RDD[Edge[Byte]] = sc.parallelize(edgeArr)

    //StorageLevel.MEMORY_ONLY MEMORY_AND_DISK_SER
    val graph = Graph(vertices, edges, null, StorageLevel.MEMORY_ONLY, StorageLevel.MEMORY_ONLY)
      .partitionBy(RandomVertexCut,5)
      .persist(StorageLevel.MEMORY_ONLY)

    val startTime2 = System.currentTimeMillis

    //-------------------------------------定义pregel处理逻辑-------------------------------------------------------------
    //pregel的迭代次数，要求大于等于1。次数为1表示节点处理完firstMessage信息后，在进行一次迭代计算
    val iterations = 2000
    //表示节点信息的发送方向
    val edgeDirection = EdgeDirection.Out
    //所有节点初始时收到的消息
    val firstMessage = new Msg()
    //每个节点对收到的消息的处理逻辑
    /***
     * vId： 当前节点的编号
     * vertexvalue： 当前节点的属性值
     * msgSum： 当前节点受到的消息集合
     */
    val updateVertex = (vId: Long, vertexValue: VertexValue, msgSum: Msg) =>
    {
      var in: Pegraph = new Pegraph
      // 空消息表示处理的是firstMessage
      if(msgSum.isEmpty) {
        // 判断是否为cfg的入口点
        if(entries.value.contains(vId))
        {
          Transfer.transfer(in, new CfgNode(vertexValue.stmt).getStmt(), grammar.value, singleton.value)

          val out = in

          var changed = false

          changed = !out.equals(vertexValue.pegraph)

          VertexValue(vertexValue.stmt, changed, out, vertexValue.graphstore)
        }
        else
        vertexValue
      }
      else
      {
        val it: (VertexId, (Boolean, String, Pegraph)) = msgSum.last

        if (msgSum.size == 1 && !it._2._1) {
          Tools.update_graphstore(vertexValue.graphstore, msgSum)

          vertexValue
        }
        else {
          Tools.update_graphstore(vertexValue.graphstore, msgSum)

          in = Tools.getIn(vertexValue.graphstore, vertexValue.stmt, grammar.value)

          Transfer.transfer(in, new CfgNode(vertexValue.stmt).getStmt(), grammar.value, singleton.value)

          val out = in

          var changed = false

          changed = !out.equals(vertexValue.pegraph)

          VertexValue(vertexValue.stmt, changed, out, vertexValue.graphstore)
        }
      }
    }

    def isFeasible(callee: Stmt, caller: Stmt, out: Pegraph, grammar: Grammar): Boolean = {
      val caller_variable: VertexId = caller.asInstanceOf[Stmt_callfptr].getDst()
      val caller_deref_variable: VertexId = caller.asInstanceOf[Stmt_callfptr].getAux()

      val callee_variable: VertexId = callee.asInstanceOf[Stmt_calleefptr].getDst()
      if (out.getGraph().contains(callee_variable)){
        val num = out.getNumEdges(callee_variable)
        val edges = out.getEdges(callee_variable)
        val labels = out.getLabels(callee_variable)

        for (i <- 0 until num){
          if (edges(i) == caller_variable || edges(i) == caller_deref_variable){
            if (grammar.isMemoryAlias(labels(i)) || grammar.isValueAlias(labels(i))){
              return true
            }
          }
        }
      }
      false
    }
    /****************************
     * a user supplied function that is applied to out edges of vertices that received messages in the current iteration
     * sendMsg: EdgeTriplet[VD, ED] => Iterator[(VertexId,A)]
     *****************************/
    val sendMsg = (triplet: EdgeTriplet[VertexValue, Byte]) =>
    {
      //triplet.dstAttr.graphstore += null
      val tmp = new Msg()
      val src_stmt = new CfgNode(triplet.srcAttr.stmt).getStmt()
      val dst_stmt = new CfgNode(triplet.dstAttr.stmt).getStmt()

      if (triplet.srcAttr.changed) {
        if (src_stmt.getType() == TYPE.Callfptr){
          if (dst_stmt.getType() == TYPE.Calleefptr){
            if (isFeasible(dst_stmt, src_stmt, triplet.srcAttr.pegraph, grammar.value)){
              tmp.put(triplet.srcId, (true, triplet.srcAttr.stmt, triplet.srcAttr.pegraph))
              Iterator((triplet.dstId, tmp))
            }
            else Iterator.empty
          }
          else {
            tmp.put(triplet.srcId, (true, triplet.srcAttr.stmt, triplet.srcAttr.pegraph))
            Iterator((triplet.dstId, tmp))
          }
        }
        else if(dst_stmt.getType() == TYPE.Return && dst_stmt.asInstanceOf[Stmt_return].getLength() == 0 &&
          (src_stmt.getType() == TYPE.Call || src_stmt.getType() == TYPE.Callfptr)) {
          tmp.put(triplet.srcId, (false, triplet.srcAttr.stmt, triplet.srcAttr.pegraph))
          Iterator((triplet.dstId, tmp))
        }
        else
          {
            tmp.put(triplet.srcId, (true, triplet.srcAttr.stmt, triplet.srcAttr.pegraph))
            Iterator((triplet.dstId, tmp))
          }
      }
      else {
        Iterator.empty
      }
    }

    //目标点聚合收到的消息
    val aggregateMsgs = (messagestore1: Msg, messagestore2: Msg) =>
    {
      messagestore1 ++= messagestore2
      messagestore1
    }

    //开始pregel的计算任务
    val resultGraph = graph.pregel(firstMessage, iterations, edgeDirection)(updateVertex, sendMsg, aggregateMsgs)

    val endTime = System.currentTimeMillis()

    println("total time:" + (endTime - startTime1))
    println("total time(no construct):" + (endTime - startTime2))

    val sum = sc.accumulator(0)
    resultGraph.vertices.foreach
    { case (id, stmt) =>
      {
        val k = stmt.pegraph
        sum += Tools.sum_edge(k)
        //println("id:" + id + " edge num: " + Tools.sum_edge(k))
      }
    }
    println(sum)
  }

}
