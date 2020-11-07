import org.apache.log4j.{Level, Logger}
import org.apache.spark.graphx.PartitionStrategy.RandomVertexCut
import org.apache.spark.{SparkConf, SparkContext, SparkFiles}
import org.apache.spark.graphx.{VertexId, _}
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel

import scala.io.Source
import collection.mutable.ArrayBuffer
import scala.collection.mutable

/*object LoadJNI
{
  lazy val load =
  {
    new Load()
  }
}*/

object Analysis
{

  def main(args: Array[String]): Unit =
  {
    //记录运行时间
    //屏蔽日志
    Logger.getLogger("org.apache.spark").setLevel(Level.WARN)
    Logger.getLogger("org.eclipse.jetty.server").setLevel(Level.OFF)

    val startTime1 = System.currentTimeMillis
    /******
     *
     * @param stmt_type : stmt's info
//     * @param flag : about entry
//     * @param label : 0 is null for pegraph
     * @param changed : about send Msg or do not send Msg
     * @param pegraph : vertex property
     * @param graphstore : previous in set
     */

    //case class StmtValue(stmt_type: String, changed: Boolean, pegraph: java.util.HashMap[java.lang.Long, EdgeArray], graphstore: java.util.HashMap[java.lang.Long, java.util.HashMap[java.lang.Long, EdgeArray]])
    case class StmtValue2(stmt_type: String, changed: Boolean, pegraph: Pegraph, graphstore: mutable.Map[VertexId, Pegraph])

    //开启Kryo的压缩
    val conf = new SparkConf()
    //graphx.GraphXUtils.registerKryoClasses(conf)
    conf.setAppName("Analysis")
    //  .set("spark.serializer", "org.apache.spark.serializer.KryoSerializer")
        .setMaster("local")
    //  .set("spark.kryo.registrationRequired", "true")
    //  .registerKryoClasses(Array(classOf[StmtValue]
    //    ,classOf[java.util.HashMap[java.lang.Long, java.util.HashMap[java.lang.Long, EdgeArray]]]
    //    ,classOf[Array[Edge[_]]]))

       // ,classOf[org.apache.spark.graphx.Edge$mcI$sp]]))

    val sc = new SparkContext(conf)

    //StmtValue： StmtValue is the type of each vertex's property
    val vertexArr = new ArrayBuffer[(VertexId, StmtValue2)]()
    //Edge[Int]: Int is the type of each edge's property
    val edgeArr = new ArrayBuffer[Edge[Byte]]()

    //sc.textFile(): Read a text file from HDFS, a local file system (available on all nodes), or any Hadoop-supported file system URI, and return it as an RDD of Strings.
    //val varbs = sc.textFile("file:///home/decxu/analysis_data/browser/final").collect()
    //varbs.foreach(print)
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

    val source_entry =  Source.fromFile("/home/decxu/Documents/analysis_data/intl/entry.txt","UTF-8")
    //val source_entry =  Source.fromFile(path_entry)
    val lines_entry = source_entry.getLines()
    val entry = mutable.Set[Long]()
    while(lines_entry.hasNext)
    {
      val en = lines_entry.next().toLong
      entry.add(en)
    }
    val entries = sc.broadcast(entry)

    val sourceE =  Source.fromFile("/home/decxu/Documents/analysis_data/intl/final","UTF-8")
    //val sourceE =  Source.fromFile(path_final)
    val linesE = sourceE.getLines()
    while(linesE.hasNext)
    {
      val ee = linesE.next().split("\t")
      edgeArr += Edge(ee(0).toLong, ee(1).toLong, 0)
    }

    //val sourceV =  Source.fromFile(path_stmt)
    val sourceV =  Source.fromFile("/home/decxu/Documents/analysis_data/intl/id_stmt_info.txt","UTF-8")
    val linesV = sourceV.getLines
    while(linesV.hasNext)
    {
      val pp = linesV.next()
      val id = pp.split("\t")(0).toLong
    //  if(entry.contains(id)) //flag为1表示该点是entry
      //put -1表示初始状态
      //vertexArr += ((id, StmtValue(pp, false, new java.util.HashMap[java.lang.Long, EdgeArray](3){put(-1l, new EdgeArray())}, new java.util.HashMap[java.lang.Long, java.util.HashMap[java.lang.Long, EdgeArray]](1))))
      vertexArr += ((id, StmtValue2(pp, false, new Pegraph(-1l), new mutable.HashMap[VertexId, Pegraph]())))
    //  else
    //    vertexArr += ((id, StmtValue(pp,  0, false, new java.util.HashMap[java.lang.Long, EdgeArray](), new java.util.HashMap[java.lang.Long, java.util.HashMap[java.lang.Long, EdgeArray]])))

    }

    val stmts: RDD[(VertexId, StmtValue2)] = sc.parallelize(vertexArr)
    val relationships: RDD[Edge[Byte]] = sc.parallelize(edgeArr)
    //StorageLevel.MEMORY_ONLY MEMORY_AND_DISK_SER
    val graph = Graph(stmts, relationships, null, StorageLevel.MEMORY_ONLY, StorageLevel.MEMORY_ONLY)
      .partitionBy(RandomVertexCut,1)
      .persist(StorageLevel.MEMORY_ONLY)

    //val sourceSingleton =  Source.fromFile(path_singleton)
    val sourceSingleton =  Source.fromFile("/home/decxu/Documents/analysis_data/intl/var_singleton_info.txt","UTF-8")
    val linesSingleton = sourceSingleton.getLines()
    var SingletonBuffer = new ArrayBuffer[Int]();
    var SingletonBuffer2 = new ArrayBuffer[VertexId]();
    while(linesSingleton.hasNext) {
      val ss = linesSingleton.next()
      SingletonBuffer += ss.toInt
      SingletonBuffer2 += ss.toLong
    }

    val singleton = sc.broadcast(new Singleton(SingletonBuffer2.toSet))
    //println(test.isSingleton(2020))
    val singletons = sc.broadcast(SingletonBuffer.toArray)

    val sourceG = Source.fromFile("/home/decxu/Documents/analysis_data/simple/rules_pointsto.txt","UTF-8")
    val linesG = sourceG.getLines()
    val grammars = new Grammar()
    while(linesG.hasNext){
      val line = linesG.next()
      grammars.loadGrammar(line)
    }
    grammars.test()
    val grammar = sc.broadcast(grammars)

    //val graph = graph1.partitionBy(RandomVertexCut,2)
    //graph.vertices.foreachPartition(element => System.loadLibrary("TestJNI"))
    //System.loadLibrary("TestJNI")
    val startTime2 = System.currentTimeMillis

    /***************************************************test code ************************************************************/
//    val new_in = new Pegraph()
//    Transfer.transfer(new_in, new CfgNode("0\talloca\t0\t2\t1").getStmt(), grammar.value, singleton.value)
//    Transfer.transfer(new_in, new CfgNode("1\talloca\t3\t5\t4").getStmt(), grammar.value, singleton.value)
//    Transfer.transfer(new_in, new CfgNode("2\talloca\t7\t12\t11").getStmt(), grammar.value, singleton.value)
//    Transfer.transfer(new_in, new CfgNode("3\tassign\t6\t0").getStmt(), grammar.value, singleton.value)
//    Transfer.transfer(new_in, new CfgNode("4\tstore\t10\t3\t6").getStmt(), grammar.value, singleton.value)
//    Transfer.transfer(new_in, new CfgNode("5\tload\t9\t10\t6").getStmt(), grammar.value, singleton.value)
//    Transfer.transfer(new_in, new CfgNode("6\tstore\t8\t7\t0").getStmt(), grammar.value, singleton.value)
//    println(new_in)







    /***************************************************test code ************************************************************/



    /****************************
     * 展示CFG图的属性
     *****************************/
    //println("\n-----------------------------")
    //Applies a function f to all elements of this RDD
    //var k = 0;
    //graph.vertices.collect().foreach{ case (id,stmt) => if(stmt.flag == 1) k += 1}//println("The stmt's id is " + id + ", content is " + stmt.flag) }
    //print(k)

    //println("\n-----------------------------")
    //graph.triplets.foreach(triplet => if(triplet.attr == 1) println( triplet.srcId + "----->" + triplet.dstId + "    attr:" + triplet.attr))

    val firstMessage = new mutable.HashMap[VertexId, Pegraph]()
    //flag.put(-1l, null)
    //val firstMessage: java.util.HashMap[java.lang.Long, java.util.HashMap[java.lang.Long, EdgeArray]] = flag

    val iterations = 3000
    val edgeDirection = EdgeDirection.Out

    //注意要判断changed，以及处理entry
    val updateVertex = (vId: Long, vData: StmtValue2, msgSum: mutable.HashMap[VertexId, Pegraph]) =>
    {
      //println("msgSum:" + msgSum.keySet())
      //处理firstMessage，
      //System.out.println(vId)
      //System.out.println("done!")
      //val in =  new java.util.HashMap[java.lang.Long, EdgeArray]()
      val in =  new Pegraph
      val out = in


      if(msgSum.isEmpty) //实现取决于没有空消息！！！！！！！！！！！！！！！！！！！！！！！！！！
      //if(msgSum.containsKey(-1l))
      {
        //println(singleton.value.isSingleton(2020))
        //System.loadLibrary("TestJNI")
        //if(vData.flag == 1)
        if(entries.value.contains(vId))//第一波判断后，不再考虑entry的相关内容
        {
          //Tool.getin(in, vData.graphstore)
          Tools.getIn(in, vData.graphstore)
          //val str = new java.lang.StringBuilder
            //将in转化为string
          //Tool.changetoString(str, in)
          //处理in转化后的string的到test
          //var test = TestJNI.transfer(str.toString, vData.stmt_type, Singleton.value)
          //val test = TestJNI.transfer("", vData.stmt_type, Singleton.value, in)
          //System.out.println(vData.stmt_type)
          //System.out.println(new CfgNode(vData.stmt_type).getStmt())
          //System.out.println("done!")
          //println(vId)
          Transfer.transfer(in, new CfgNode(vData.stmt_type).getStmt(), grammar.value, singleton.value)
          //print("in: ")
          //println(in)
          //println(in.getGraph()(7951).getSize())
          //TestJNI.transfer(vData.stmt_type, singletons.value, in)
          //System.out.println(test)
          //Tool.print(out);

          //val str = new java.lang.StringBuilder
          //Tool.changetoString(str, out)
          //System.out.println(str.toString)
            //将test转化为out
          //Tool.changetoMap(out, test)
          //Tool.print(out);


          //var changed = false
          //changed = !Tool.isEquals(out, vData.pegraph)

          //println("old graphstore :" + changed + vData.graphstore.keySet())
          //第一次必定改变了，即change为true
          //val vData2 = StmtValue(vData.stmt_type, true, out, vData.graphstore)
          //vData2
          StmtValue2(vData.stmt_type, true, out, vData.graphstore)
        }
        else
        vData
      }
      //同时，如果out有变化，将changed赋值为true
      else
      {
        //更新graphstore
        //Tool.update_graphstore(vData.graphstore, msgSum)
        Tools.update_graphstore(vData.graphstore, msgSum)
        //val in = new java.util.HashMap[java.lang.Long, EdgeArray]()
        Tools.getIn(in, vData.graphstore)
        //val str = new java.lang.StringBuilder
        //将in转化为string
        //Tool.changetoString(str, in)
        //处理in转化后的string的到test
        //val tmp = new Array[Int](0)
        //第一次传入singleton后无需再进行传输
        //val test = TestJNI.transfer("", vData.stmt_type, in)
        //直接对in进行更新，也即是out
        //println(vId)
        //println("test!")
        Transfer.transfer(in, new CfgNode(vData.stmt_type).getStmt(), grammar.value, singleton.value)
        //print("in: ")
        //println(in)
        //TestJNI.transfer(vData.stmt_type, in)
        //var test = TestJNI.transfer(str.toString, vData.stmt_type, Singleton.value)
        //val out = new java.util.HashMap[java.lang.Long, EdgeArray]()
        //将test转化为out
        //Tool.changetoMap(out, test)

        var changed = false
        //changed = !Tool.isEquals(out, vData.pegraph)
        changed = !out.equals(vData.pegraph)
        //changed = !Tools.isEquals(out, vData.pegraph)

        //println("old graphstore :" + changed + vData.graphstore.keySet())
        StmtValue2(vData.stmt_type,changed, out, vData.graphstore)
        //val vData2 =
        //vData2
      }
    }

    /****************************
     * a user supplied function that is applied to out edges of vertices that received messages in the current iteration
     * sendMsg: EdgeTriplet[VD, ED] => Iterator[(VertexId,A)]
     *****************************/
    val sendMsg = (triplet: EdgeTriplet[StmtValue2, Byte]) =>
    {
      //if(triplet.srcAttr.flag == 1) //第一次迭代，所有的顶点都是active，只有是entry的才可以发送消息，之后的迭代不在考虑entry的问题
      //{
        if(triplet.srcAttr.changed)
        {
          //如果changed，需要发送满足消息结构的（java.util.HashMap[Long, PEgraph]）包含pegraph的内容
          //深拷贝
          //val tmp = new java.util.HashMap[java.lang.Long, java.util.HashMap[java.lang.Long, EdgeArray]]()
          val tmp = new mutable.HashMap[VertexId, Pegraph]()
         // val pegraph = new java.util.HashMap[java.lang.Long, EdgeArray]()
         // Tool.dcopy(pegraph, triplet.srcAttr.pegraph)

          //将拷贝结果发送出去
         // tmp.put(triplet.srcId, pegraph)
          //无需深拷贝
          tmp.put(triplet.srcId, triplet.srcAttr.pegraph)
          Iterator((triplet.dstId, tmp))
        }
        else
        {
          Iterator.empty
        }
      //}
      //else
      //{
      //  Iterator.empty
      //}
    }

    val aggregateMsgs = (messagestore1: mutable.HashMap[VertexId, Pegraph], messagestore2: mutable.HashMap[VertexId, Pegraph]) =>
    {
      //println("messagestore1:" + messagestore1.keySet())
      //println("messagestore2:" + messagestore2.keySet())
      messagestore1 ++= messagestore2
      //messagestore1.putAll(messagestore2);
      //println("after update messagestore1:" + messagestore1.keySet())
      //messagestore1
    }
    //start superstep
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
        //println("id:" + id + " edge num: " + Tool.sum_edge(k))
      }
    }
    println(sum)
    //println("edges NumPartitions: " + graph.edges.getNumPartitions)
    //println("vertex NumPartitions: " + graph.vertices.getNumPartitions)
  }

}
