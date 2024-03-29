import org.apache.spark.graphx.VertexId
import stmt.TYPE.TYPE
import stmt.{Stmt, Stmt_alloc, Stmt_assign, Stmt_load, Stmt_phi, Stmt_store, TYPE}
import util.control.Breaks._

import scala.collection.mutable

object Transfer{

  def transfer(in: Pegraph, stmt: Stmt, grammar: Grammar, singletons: Singleton ) = {
    stmt.getType() match{
      case TYPE.Assign => {
        transfer_copy(in, stmt.asInstanceOf[Stmt_assign], grammar, singletons)
      }
      case TYPE.Load => {
        transfer_load(in, stmt.asInstanceOf[Stmt_load], grammar, singletons)
      }
      case TYPE.Store => {
        transfer_store(in, stmt.asInstanceOf[Stmt_store], grammar, singletons)
      }
      case TYPE.Alloca => {
        transfer_address(in, stmt.asInstanceOf[Stmt_alloc], grammar, singletons)
      }
      case TYPE.Phi => {
        transfer_phi(in, stmt.asInstanceOf[Stmt_phi], grammar, singletons)
      }
      case TYPE.Call => {
        transfer_Call(in)
      }
      case TYPE.Return => {
        transfer_Return(in)
      }
      case TYPE.Ret => {
        transfer_Ret(in)
      }
      case TYPE.Calleefptr => {
        println(stmt.toString)
        transfer_Calleefptr(in)
      }
      case TYPE.Callfptr => {
        println(stmt.toString)
        transfer_Callfptr(in)
      }
      case TYPE.Skip => {
        println(stmt.toString)
        transfer_Skip(in)
      }
      case _ => {
        println("error stmt type for transfer function!")
        System.exit(2)
      }
    }
  }

  def is_strong_update_aux(aux: VertexId, out: Pegraph, grammar: Grammar, singleton: Singleton): Boolean = {
    /* If there exists one and only one variable o, which
     * refers to a singleton memory location,such that x points to o
     */
    if (!out.getGraph().contains(aux))
      false
    else {
      var numOfSingleTon = 0
      val numEdges = out.getNumEdges(aux)
      val edges = out.getEdges(aux)
      val labels = out.getLabels(aux)

      for(i <- 0 until numEdges) {
        if(grammar.isPointsTo(labels(i)) && singleton.isSingleton(edges(i))) {
          numOfSingleTon += 1
        }
      }
      numOfSingleTon == 1
    }
  }

  def must_alias_store_aux(aux: VertexId, x: VertexId, out: Pegraph, vertices_changed: mutable.Set[VertexId], grammar: Grammar, vertices_affected: mutable.Set[VertexId], singleton: Singleton) = {
    assert(!out.getGraph().contains(x))
    assert(out.getGraph().contains(aux))

    /* if there exists one and only one variable o,which
    * refers to a singleton memory location,such that x and
    * y are both memory aliases of o,then x and y are Must-alias
     */
    val set1 = mutable.Set.empty[VertexId]

    {
      val numEdges = out.getNumEdges(aux)
      val edges = out.getEdges(aux)
      val labels = out.getLabels(aux)

      for (i <- 0 until numEdges) {
        if (grammar.isPointsTo(labels(i)) && singleton.isSingleton(edges(i))) set1.add(edges(i))
      }
    }
    assert(set1.size == 1)

    //compute all the must-alias expressions
    val numEdges = out.getNumEdges(aux)
    val edges = out.getEdges(aux)
    val labels = out.getLabels(aux)

    for (i <- 0 until numEdges) {
      if (grammar.isPointsTo(labels(i))) {
        val set2 = mutable.Set.empty[VertexId]

        val candidate = edges(i)
        val numEdgess = out.getNumEdges(candidate)
        val edgess = out.getEdges(candidate)
        val labelss = out.getLabels(candidate)

        for (i <- 0 until numEdgess) {
          if (grammar.isMemoryAlias(labelss(i)) && singleton.isSingleton(edgess(i))) set2.add(edgess(i))
        }

        if((set2.size == 1) && (set2(0) == set1(0))) {
          vertices_changed.add(candidate)
        }
      }
    }
    vertices_changed.add(x)

    //add *x into vertices as well
    for (it <- vertices_changed) {
      val x = it
      val numEdges = out.getNumEdges(x)
      val edgess = out.getEdges(x)
      val labelss = out.getLabels(x)

      for (i <- 0 until numEdges) {
        if(grammar.isDereference(labelss(i))) {
          vertices_changed.add(edgess(i))
        }
        if (grammar.isDereference_reverse(labelss(i))) {
          vertices_affected.add(edgess(i))
        }
      }
    }
  }

  def strong_update_store_aux_simplify(aux: VertexId, x: VertexId, out: Pegraph, vertices_changed: mutable.Set[VertexId], grammar: Grammar, vertices_affected: mutable.Set[VertexId], singleton: Singleton) = {
    assert(!out.getGraph().contains(x))
    assert(out.getGraph().contains(aux))

    // vertices <- must_alias(x); put *x into this set as well
    must_alias_store_aux(aux, x, out, vertices_changed, grammar, vertices_affected, singleton)

    /* remove edges */
    for (it <- out.getGraph()) {
      breakable {
        if (it._2.isEmpty()) {
          break()
        }
        val src = it._1
        /* delete all the ('a', '-a', 'V', 'M', and other temp labels) edges associated with a vertex within vertices_changed, and
         * all the ('V', 'M', and other temp labels) edges associated with that within vertices_affected
         * */
        val deletedArray = new EdgeArray()
        findDeletedEdges(it._2, src, vertices_changed, vertices_affected, grammar, deletedArray)

        if (deletedArray.getSize() != 0) {
          val n1 = out.getNumEdges(src)
          val n2 = deletedArray.getSize()
          val edges = new Array[VertexId](n1)
          val labels = new Array[Byte](n1)
          val len = myalgo.minusTwoArray(edges, labels, n1, out.getEdges(src), out.getLabels(src), n2, deletedArray.getEdges(), deletedArray.getLabels())

          if (len != 0) {
            out.setEdgeArray(src,len,edges,labels)
          }
          else {
            out.getGraph() -= it._1
          }
        }
      }
    }
  }

  def transfer_Ret(out: Pegraph) = out

  def transfer_Return(out: Pegraph) = out

  def transfer_Call(out: Pegraph) = out

  def transfer_Calleefptr(out: Pegraph) = out

  def transfer_Callfptr(out: Pegraph) = out

  def transfer_Skip(out: Pegraph) = out

  def transfer_phi(out: Pegraph, stmt: Stmt_phi, grammar: Grammar, singletons: Singleton) = {
    // the KILL set
    val vertices_changed = mutable.Set.empty[VertexId]
    val vertices_affected = mutable.Set.empty[VertexId]

    strong_update_simplify(stmt.getDst(), out, vertices_changed, grammar, vertices_affected, singletons)
    // the GEN set
    peg_compute_add(out, stmt, grammar)
  }

  def transfer_load(out: Pegraph, stmt: Stmt_load, grammar: Grammar, singletons: Singleton): Unit = {
    // the KILL set
    val vertices_changed = mutable.Set.empty[VertexId]
    val vertices_affected = mutable.Set.empty[VertexId]

    strong_update_simplify(stmt.getDst(), out, vertices_changed, grammar, vertices_affected, singletons)

    // the GEN set
    peg_compute_add(out, stmt, grammar)
  }

  def is_strong_update_dst(x: VertexId, out: Pegraph, grammar: Grammar, singletons: Singleton): Boolean = {
    /* If there exists one and only one variable o,which
     * refers to a singleton memory location,such that x and o are memory alias
     */
    assert(out.getGraph().contains(x))

    var numOfSingleTon = 0
    val numEdges = out.getNumEdges(x)
    val edges = out.getEdges(x)
    val labels = out.getLabels(x)

    for (i <- 0 until numEdges){
      if(grammar.isMemoryAlias(labels(i)) && singletons.isSingleton(edges(i)))  numOfSingleTon += 1
    }

    numOfSingleTon == 1
  }

  def must_alias_store_dst(x: VertexId, out: Pegraph, vertices_changed: mutable.Set[VertexId], grammar: Grammar, vertices_affected: mutable.Set[VertexId], singletons: Singleton) = {
    /* if there exists one and only one variable o,which
	 * refers to a singleton memory location,such that x and
	 * y are both memory aliases of o,then x and y are Must-alias
	 */
    val set1 = mutable.Set.empty[VertexId]
    assert(!singletons.isSingleton(x))

    {
      val numEdges = out.getNumEdges(x)
      val edges = out.getEdges(x)
      val labels = out.getLabels(x)

      for (i <- 0 until numEdges) {
        if (grammar.isMemoryAlias(labels(i)) && singletons.isSingleton(edges(i))) {
          set1 += edges(i)
        }
      }
    }

    assert(set1.size == 1)

    //compute all the must-alias expressions
    val numEdges = out.getNumEdges(x)
    val edges = out.getEdges(x)
    val labels = out.getLabels(x)

    for (i <- 0 until numEdges) {
      if (grammar.isMemoryAlias(labels(i))) {
        val set2 = mutable.Set.empty[VertexId]

        val candidate = edges(i)
        val numEdgess = out.getNumEdges(candidate)
        val edgess = out.getEdges(candidate)
        val labelss = out.getLabels(candidate)

        for (i <- 0 until numEdgess) {
          if (grammar.isMemoryAlias(labelss(i)) && singletons.isSingleton(edgess(i))) {
            set2 += edgess(i)
          }
        }
        if (set2.size == 1 && set2(0) == set1(0)) vertices_changed += candidate
      }
    }
    vertices_changed += x
    //add *x into vertices as well
    for (it <- vertices_changed) {
      val x = it

      val numEdges = out.getNumEdges(x)
      val edges = out.getEdges(x)
      val labels = out.getLabels(x)

      for (i <- 0 until numEdges) {
        if (grammar.isDereference(labels(i))) {
          vertices_changed += edges(i)
        }

        if (grammar.isDereference_reverse(labels(i))) {
          vertices_affected += edges(i)
        }
      }
    }
  }

  def strong_update_store_dst_simplify(x: VertexId, out: Pegraph, vertices_changed: mutable.Set[VertexId], grammar: Grammar, vertices_affected: mutable.Set[VertexId], singletons: Singleton) = {
    // vertices <- must_alias(x); put *x into this set as well
    must_alias_store_dst(x, out, vertices_changed, grammar, vertices_affected, singletons)

    /* remove edges */
    for (it <- out.getGraph() if !it._2.isEmpty()) {
      val src = it._1
      /* delete all the ('a', '-a', 'V', 'M', and other temp labels) edges associated with a vertex within vertices_changed, and
       * all the ('V', 'M', and other temp labels) edges associated with that within vertices_affected
       * */
      val deletedArray = new EdgeArray()
      findDeletedEdges(it._2, src, vertices_changed, vertices_affected, grammar, deletedArray)

      if (deletedArray.getSize() != 0) {
        val n1 = out.getNumEdges(src)
        val n2 = deletedArray.getSize()
        val edges = new Array[VertexId](n1)
        val labels = new Array[Byte](n1)
        val len = myalgo.minusTwoArray(edges, labels, n1, out.getEdges(src), out.getLabels(src), n2, deletedArray.getEdges(), deletedArray.getLabels())
        if (len != 0) out.setEdgeArray(src,len,edges,labels)
        else out.getGraph() -= it._1
      }
    }
  }

  def transfer_store(out: Pegraph, stmt: Stmt_store, grammar: Grammar, singletons: Singleton): Unit = {
    // the KILL set
    val vertices_changed = mutable.Set.empty[VertexId]
    val vertices_affected = mutable.Set.empty[VertexId]

    if (out.getGraph().contains(stmt.getDst())) {
      if (is_strong_update_dst(stmt.getDst(), out, grammar, singletons)) {
        strong_update_store_dst_simplify(stmt.getDst(),out, vertices_changed, grammar, vertices_affected, singletons)
      }
    }
    else {
      if (is_strong_update_aux(stmt.getAux(), out, grammar, singletons)) {
        strong_update_store_aux_simplify(stmt.getAux(), stmt.getDst(), out, vertices_changed, grammar, vertices_affected, singletons)
      }
    }
    peg_compute_add(out, stmt, grammar)
  }

  def transfer_address(out: Pegraph, stmt: Stmt_alloc, grammar: Grammar, singletons: Singleton): Unit = {
    // the KILL set
    val vertices_changed = mutable.Set.empty[VertexId]
    val vertices_affected = mutable.Set.empty[VertexId]

    strong_update_simplify(stmt.getDst(), out, vertices_changed, grammar, vertices_affected, singletons)

    // the GEN set
    peg_compute_add(out, stmt, grammar)
  }

  def transfer_copy(out: Pegraph, stmt: Stmt_assign, grammar: Grammar, singletons: Singleton) = {
    val vertices_changed = mutable.Set.empty[VertexId]
    val vertices_affected = mutable.Set.empty[VertexId]

    strong_update_simplify(stmt.getDst(), out, vertices_changed, grammar, vertices_affected, singletons)
    // the GEN set
    peg_compute_add(out, stmt, grammar)
  }

  def strong_update_simplify(x: VertexId, out: Pegraph, vertices_changed: mutable.Set[VertexId], grammar: Grammar, vertices_affected: mutable.Set[VertexId], singletons: Singleton): Unit = {
    if(out.getGraph().contains(x)) {
      // vertices <- must_alias(x); put *x into this set as well
      must_alias(x, out, vertices_changed, grammar, vertices_affected, singletons)

      /* remove edges */
      for (it <- out.getGraph() if !it._2.isEmpty()) {
        val src: VertexId = it._1

        /* delete all the ('a', '-a', 'V', 'M', and other temp labels) edges associated with a vertex within vertices_changed, and
           * all the ('V', 'M', and other temp labels) edges associated with that within vertices_affected
           * */
        val deletedArray: EdgeArray = new EdgeArray()
        findDeletedEdges(it._2, src, vertices_changed, vertices_affected, grammar, deletedArray)

        if (deletedArray.getSize() != 0) {
          val n1: Int = out.getNumEdges(src)
          val n2: Int = deletedArray.getSize()
          val edges = new Array[VertexId](n1)
          val labels = new Array[Byte](n1)
          val len: Int = myalgo.minusTwoArray(edges, labels, n1, out.getEdges(src), out.getLabels(src), n2, deletedArray.getEdges(), deletedArray.getLabels())
          if (len != 0) out.setEdgeArray(src, len, edges, labels)
          else out.getGraph() -= it._1
        }
      }
    }
  }

  def getDirectAddedEdges_phi(out: Pegraph, stmt_tmp: Stmt, grammar: Grammar, m: mutable.Map[VertexId, EdgeArray], flag: Boolean) = {
    val stmt = stmt_tmp.asInstanceOf[Stmt_phi]

    //'a', '-a', 'd', '-d', and self-loop edges
    val length = stmt.getLength()
    val srcs = stmt.getSrcs()
    val dst = stmt.getDst()
    val edges_dst = new EdgeArray()

    for (i <- 0 until length) {
      val src = srcs(i)
      val edges_src = new EdgeArray()
      //'a', '-a'
      edges_src.addOneEdge(dst, grammar.getLabelValue(Array('a')))
      edges_dst.addOneEdge(src, grammar.getLabelValue(Array('-','a')))

      //merge and sort
      edges_src.merge()

      //remove the exiting edges
      removeExistingEdges(edges_src, src, out ,m)
    }

    if (!flag) {
      for (i <- 0 until grammar.getNumErules()) {
        val label = grammar.getErule(i)
        edges_dst.addOneEdge(dst, label)
      }
    }
    edges_dst.merge()
    removeExistingEdges(edges_dst, dst, out, m)
  }

  def removeExistingEdges(edges_src: EdgeArray, src: VertexId, out: Pegraph, m: mutable.Map[VertexId, EdgeArray]) = {
    //remove the existing edges
    val n1: Int = edges_src.getSize()
    val edges = new Array[VertexId](n1)
    val labels = new Array[Byte](n1)

    val len = myalgo.minusTwoArray(edges, labels, edges_src.getSize(), edges_src.getEdges(),
      edges_src.getLabels(), out.getNumEdges(src), out.getEdges(src), out.getLabels(src))

    if (len != 0) {
      m(src) = new EdgeArray()
      m(src).set(len, edges, labels)
    }
  }

  def getDirectAddedEdges_alloc(out: Pegraph, stmt: Stmt_alloc, grammar: Grammar, m: mutable.Map[VertexId, EdgeArray], flag: Boolean) = {
    //'a', '-a', 'd', '-d', and self-loop edges
    val src: VertexId = stmt.getSrc()
    val edges_src = new EdgeArray()
    val dst: VertexId = stmt.getDst()
    val edges_dst = new EdgeArray()
    val aux: VertexId = stmt.getAux()
    val edges_aux = new EdgeArray()

    //'a', '-a'
    edges_src.addOneEdge(dst, grammar.getLabelValue(Array('a')))
    edges_dst.addOneEdge(src, grammar.getLabelValue(Array('-','a')))

    //'d', '-d'
    edges_src.addOneEdge(aux, grammar.getLabelValue(Array('d')))
    edges_aux.addOneEdge(src, grammar.getLabelValue(Array('-','d')))

    if (!flag) {
      for (i <- 0 until grammar.getNumErules()) {
        val label = grammar.getErule(i)
        edges_src.addOneEdge(src, label)
        edges_dst.addOneEdge(dst, label)
        edges_aux.addOneEdge(aux, label)
      }
    }
    //merge and sort//merge and sort
    edges_src.merge()
    edges_dst.merge()
    edges_aux.merge()

    //remove the existing edges
    removeExistingEdges(edges_src, src, out, m)
    removeExistingEdges(edges_dst, dst, out, m)
    removeExistingEdges(edges_aux, aux, out, m)
  }

  def getDirectAddedEdges_store(out: Pegraph, stmt: Stmt_store, grammar: Grammar, m: mutable.Map[VertexId, EdgeArray], flag: Boolean) = {
    //'a', '-a', 'd', '-d', and self-loop edges
    val src: VertexId = stmt.getSrc()
    val edges_src = new EdgeArray()
    val dst: VertexId = stmt.getDst()
    val edges_dst = new EdgeArray()
    val aux: VertexId = stmt.getAux()
    val edges_aux = new EdgeArray()

    //'a', '-a'
    edges_src.addOneEdge(dst, grammar.getLabelValue(Array('a')))
    edges_dst.addOneEdge(src, grammar.getLabelValue(Array('-','a')))

    //'d', '-d'
    edges_aux.addOneEdge(dst, grammar.getLabelValue(Array('d')))
    edges_dst.addOneEdge(aux, grammar.getLabelValue(Array('-','d')))

    if (!flag) {
      for (i <- 0 until grammar.getNumErules()) {
        val label = grammar.getErule(i)
        edges_src.addOneEdge(src, label)
        edges_dst.addOneEdge(dst, label)
        edges_aux.addOneEdge(aux, label)
      }
    }
    //merge and sort//merge and sort
    edges_src.merge()
    edges_dst.merge()
    edges_aux.merge()

    //remove the existing edges
    removeExistingEdges(edges_src, src, out, m)
    removeExistingEdges(edges_dst, dst, out, m)
    removeExistingEdges(edges_aux, aux, out, m)
  }

  def getDirectAddedEdges_load(out: Pegraph, stmt: Stmt_load, grammar: Grammar, m: mutable.Map[VertexId, EdgeArray], flag: Boolean) = {
    //'a', '-a', 'd', '-d', and self-loop edges
    val src: VertexId = stmt.getSrc()
    val edges_src = new EdgeArray()
    val dst: VertexId = stmt.getDst()
    val edges_dst = new EdgeArray()
    val aux: VertexId = stmt.getAux()
    val edges_aux = new EdgeArray()

    //'a', '-a'
    edges_src.addOneEdge(dst, grammar.getLabelValue(Array('a')))
    edges_dst.addOneEdge(src, grammar.getLabelValue(Array('-','a')))

    //'d', '-d'
    edges_aux.addOneEdge(src, grammar.getLabelValue(Array('d')))
    edges_src.addOneEdge(aux, grammar.getLabelValue(Array('-','d')))

    if (!flag) {
      for (i <- 0 until grammar.getNumErules()) {
        val label = grammar.getErule(i)
        edges_src.addOneEdge(src, label)
        edges_dst.addOneEdge(dst, label)
        edges_aux.addOneEdge(aux, label)
      }
    }
    //merge and sort
    edges_src.merge()
    edges_dst.merge()
    edges_aux.merge()

    //remove the existing edges
    removeExistingEdges(edges_src, src, out, m)
    removeExistingEdges(edges_dst, dst, out, m)
    removeExistingEdges(edges_aux, aux, out, m)
  }

  def getDirectAddedEdges_assign(out: Pegraph, stmt: Stmt_assign, grammar: Grammar, m: mutable.Map[VertexId, EdgeArray], flag: Boolean) = {
    //'a', '-a', 'd', '-d', and self-loop edges
    val src: VertexId = stmt.getSrc()
    val edges_src = new EdgeArray()
    val dst: VertexId = stmt.getDst()
    val edges_dst = new EdgeArray()
    //'a', '-a'
    edges_src.addOneEdge(dst, grammar.getLabelValue(Array('a')))
    edges_dst.addOneEdge(src, grammar.getLabelValue(Array('-','a')))
    //self-loop edges
    if (!flag) {
      for (i <- 0 until grammar.getNumErules()) {
        val label = grammar.getErule(i)
        edges_src.addOneEdge(src, label)
        edges_dst.addOneEdge(dst, label)
      }
    }
    //merge and sort
    edges_src.merge()
    edges_dst.merge()

    //remove the existing edges
    removeExistingEdges(edges_src, src, out, m)
    removeExistingEdges(edges_dst, dst, out, m)
  }

  def getDirectAddedEdges(out: Pegraph, stmt: Stmt, grammar: Grammar, m: mutable.Map[VertexId, EdgeArray], flag: Boolean) = {
    val t: TYPE = stmt.getType()
    if (t == TYPE.Alloca) {
      val stmt_alloc = stmt.asInstanceOf[Stmt_alloc]
      getDirectAddedEdges_alloc(out, stmt_alloc, grammar, m, flag)
    }
    else if (t == TYPE.Store) {
      val stmt_store = stmt.asInstanceOf[Stmt_store]
      getDirectAddedEdges_store(out, stmt_store, grammar, m, flag)
    }
    else if (t == TYPE.Load) {
      val stmt_load = stmt.asInstanceOf[Stmt_load]
      getDirectAddedEdges_load(out, stmt_load, grammar, m, flag)
    }
    else if (t == TYPE.Assign) {
      val stmt_assign = stmt.asInstanceOf[Stmt_assign]
      getDirectAddedEdges_assign(out, stmt_assign, grammar, m, flag)
    }
    else {
      println("wrong stmt type!!!！")
      System.exit(3)
    }
  }

  def genS_RuleEdges_delta(index: VertexId, compset: ComputationSet, containers: ArraysToMerge, grammar: Grammar) = {
    val numEdges: VertexId = compset.getDeltasNumEdges(index) //## can we make sure that the deltas is uniqueness
    val edges: Array[VertexId] = compset.getDeltasEdges(index)
    val labels: Array[Byte] = compset.getDeltasLabels(index)

    var newLabel: Byte = labels(0)
    var added = false
    for (i <- 0 until numEdges.toInt) {
      newLabel = grammar.checkRules(labels(i))
      if (newLabel != 127) {
        if (!added) {
          containers.addOneContainer()
          added = true
        }
        containers.addOneEdge(edges(i), newLabel)
      }
    }
  }

  def genD_RuleEdges_delta(index: VertexId, compset: ComputationSet, containers: ArraysToMerge, grammar: Grammar) = {
    val numEdges_src_delta: VertexId = compset.getDeltasNumEdges(index) //## can we make sure that the deltas is uniqueness
    val edges_src_delta: Array[VertexId] = compset.getDeltasEdges(index)
    val labels_src_delta: Array[Byte] = compset.getDeltasLabels(index)

    for (i_src <- 0 until numEdges_src_delta.toInt) {
      val dstId = edges_src_delta(i_src)
      val dstVal = labels_src_delta(i_src)

      //delta * delta
      if (compset.getDeltas().contains(dstId)) {
        val numEdges_delta = compset.getDeltasNumEdges(dstId)
        val edges_delta = compset.getDeltasEdges(dstId)
        val labels_delta = compset.getDeltasLabels(dstId)

        var newVal: Byte = 0
        var added = false
        for (i <- 0 until numEdges_delta.toInt) {
          newVal = grammar.checkRules(dstVal, labels_delta(i))
          if (newVal != 127) {
            if(!added){
              containers.addOneContainer()
              added = true
            }
            containers.addOneEdge(edges_delta(i), newVal)
          }
        }
      }
      //delta * old
      if (compset.getOlds().contains(dstId)) {
        val numEdges_old = compset.getOldsNumEdges(dstId)
        val edges_old = compset.getOldsEdges(dstId)
        val labels_old = compset.getOldsLabels(dstId)
        var newVal: Byte = 0
        var added = false
        for (i <- 0 until numEdges_old.toInt ) {
          newVal = grammar.checkRules(dstVal, labels_old(i.toInt))
          if (newVal != 127) {
            if (!added) {
              containers.addOneContainer()
              added = true
            }
            containers.addOneEdge(edges_old(i.toInt), newVal)
          }
        }
      }
    }
  }

  def genD_RuleEdges_old(index: VertexId, compset: ComputationSet, containers: ArraysToMerge, grammar: Grammar) = {
    val numEdges_src_old: VertexId = compset.getOldsNumEdges(index) //## can we make sure that the deltas is uniqueness
    val edges_src_old: Array[VertexId] = compset.getOldsEdges(index)
    val labels_src_old: Array[Byte] = compset.getOldsLabels(index)

    for (i_src <- 0 until numEdges_src_old.toInt) {
      val dstId = edges_src_old(i_src.toInt)
      val dstVal = labels_src_old(i_src.toInt)

      breakable {
        if(!compset.getDeltas.contains(dstId)) {
          break()
        }

        val numEdges_delta = compset.getDeltasNumEdges(dstId)
        val edges_delta = compset.getDeltasEdges(dstId)
        val labels_delta = compset.getDeltasLabels(dstId)

        var newVal: Byte = 0
        var added = false
        for (i <- 0 until numEdges_delta.toInt ) {
          newVal = grammar.checkRules(dstVal, labels_delta(i.toInt))
          if (newVal != 127) {
            if (!added) {
              containers.addOneContainer()
              added = true
            }
            containers.addOneEdge(edges_delta(i.toInt), newVal)
          }
        }
      }
    }
  }

  def getEdgesToMerge(index: VertexId, compset: ComputationSet, oldEmpty: Boolean, deltaEmpty: Boolean, containers: ArraysToMerge, grammar: Grammar) = {
    // add s-rule edges
    if (!deltaEmpty) {
      genS_RuleEdges_delta(index, compset, containers, grammar)

      genD_RuleEdges_delta(index, compset, containers, grammar)
    }
    if (!oldEmpty) {
      genD_RuleEdges_old(index, compset, containers, grammar)
    }
  }

  def computeOneVertex(index: VertexId, compset: ComputationSet, grammar: Grammar): Long = {
    val oldEmpty = (compset.oldEmpty(index) || compset.getOlds()(index).isEmpty())
    val deltaEmpty = (compset.deltaEmpty(index) || compset.getDeltas()(index).isEmpty())
    // if this vertex has no edges, no need to merge.
    if (oldEmpty && deltaEmpty) {
      return 0
    }
    // use array
    val containers = new ArraysToMerge()
    // find new edges to containers
    getEdgesToMerge(index, compset, oldEmpty, deltaEmpty, containers, grammar)

    containers.merge()

    val newEdgesNum: Int = containers.getNumEdges()

    if (newEdgesNum != 0) {
      compset.setNews(index, newEdgesNum, containers.getEdgesFirstAddr(), containers.getLabelsFirstAddr())
    }
    newEdgesNum
  }

  def computeOneIteration(compset: ComputationSet, grammar: Grammar) = {
    val vertexSet = compset.getVertices()

    for (it <- vertexSet) computeOneVertex(it, compset, grammar)
  }

  def mergeToDeletedGraph(i_new: VertexId, compset: ComputationSet) = {
    println("use mergeToDeletedGraph but not implete!")
  }

  def postProcessOneIteration(compset: ComputationSet, isDelete: Boolean) = {
    for (it <- compset.getOlds()) {
      val id_old: VertexId = it._1
      if (compset.getDeltas().contains(id_old)) {
        val n1 = compset.getOldsNumEdges(id_old).toInt
        val n2 = compset.getDeltasNumEdges(id_old).toInt
        val edges = new Array[VertexId](n1 + n2)
        val labels = new Array[Byte](n1 + n2)
        val len = myalgo.unionTwoArray(edges, labels, n1, compset.getOldsEdges(id_old),
          compset.getOldsLabels(id_old), n2, compset.getDeltasEdges(id_old), compset.getDeltasLabels(id_old))
        compset.setOlds(id_old, len, edges, labels)

        compset.getDeltas() -= id_old
      }
    }

    for (it <- compset.getDeltas()) {
      val id_delta: VertexId = it._1
      //the left in deltas doesn't exist in olds
      assert(!compset.getOlds().contains(id_delta))
      compset.setOlds(id_delta, compset.getDeltasNumEdges(id_delta).toInt, compset.getDeltasEdges(id_delta), compset.getDeltasLabels(id_delta))
      compset.getDeltas() -= it._1
    }
    assert(compset.getDeltas().isEmpty)

    for (it <- compset.getNews()) {
      val i_new: VertexId = it._1

      if (isDelete) {
        mergeToDeletedGraph(i_new, compset);
      }
      val n1 = compset.getNewsNumEdges(i_new).toInt
      val n2 = compset.getOldsNumEdges(i_new).toInt
      val edges = new Array[VertexId](n1)
      val labels = new Array[Byte](n1)
      val len = myalgo.minusTwoArray(edges, labels, n1, compset.getNewsEdges(i_new),
        compset.getNewsLabels(i_new), n2, compset.getOldsEdges(i_new), compset.getOldsLabels(i_new))
      if (len != 0) {
        compset.setDeltas(i_new, len, edges, labels)
      }
      compset.getNews() -= it._1
    }
  }

  def startCompute_add(compset: ComputationSet, grammar: Grammar): Long = {
    var totalAddedEdges: Long = 0
    breakable (
      while (true) {
        computeOneIteration(compset, grammar)

        postProcessOneIteration(compset, false)

        val realAddedEdgesPerIter: Long = compset.getDeltasTotalNumEdges()

        totalAddedEdges += realAddedEdgesPerIter

        if (realAddedEdgesPerIter == 0)
          break()
      }
    )
    totalAddedEdges
  }

  def peg_compute_add(out: Pegraph, stmt: Stmt, grammar: Grammar): AnyVal = {
    val isConservative = true
    val m = mutable.Map.empty[VertexId, EdgeArray]
    if (stmt.getType() == TYPE.Phi) {
      getDirectAddedEdges_phi(out, stmt, grammar, m, false)
    }
    else {
      getDirectAddedEdges(out, stmt, grammar, m, false)
    }

    if (m.isEmpty && !isConservative) { // no new edges directly added
      return
    }

    // add assign edge based on stmt, (out,assign edge) -> compset
    val compset: ComputationSet = new ComputationSet()

    compset.init_add(out, m, isConservative)

    // start GEN
    startCompute_add(compset, grammar)
    // GEN finished, compset -> out
    val olds = compset.getOlds()

    for (it <- olds) {
      val id = it._1
      if (!out.getGraph().contains(id)) {
        out.setEdgeArray(id, it._2.getSize(), it._2.getEdges(), it._2.getLabels())
      }
      else if (out.getNumEdges(id) != it._2.getSize()) {
        out.setEdgeArray(id, it._2.getSize(), it._2.getEdges(), it._2.getLabels())
      }
    }
  }

  def must_alias(x: VertexId, out: Pegraph, vertices_changed: mutable.Set[VertexId], grammar: Grammar, vertices_affected: mutable.Set[VertexId], singletons: Singleton): Unit = {
    /* if there exists one and only one variable o,which
	 * refers to a singleton memory location,such that x and
	 * y are both memory aliases of o,then x and y are Must-alias
	 */
    val set1: mutable.Set[VertexId] = mutable.Set.empty[VertexId]
    set1 += x

    //compute all the must-alias expressions
    val numEdges: Int = out.getNumEdges(x)
    val edges: Array[VertexId] = out.getEdges(x)
    val labels: Array[Byte] = out.getLabels(x)

    for (i <- 0 until numEdges) {
      if (grammar.isMemoryAlias(labels(i))) {
        val set2: mutable.Set[VertexId] = mutable.Set.empty[VertexId]

        val candidate: VertexId = edges(i)
        val numEdgess = out.getNumEdges(candidate)
        val edgess = out.getEdges(candidate)
        val labelss = out.getLabels(candidate)

        for (k <- 0 until numEdgess) {
          if (grammar.isMemoryAlias(labelss(k)) && singletons.isSingleton(edgess(k))) {
            set2 += edgess(k)
          }
        }

        if (set2.size == 1 && set2(0) == set1(0)) {
          vertices_changed += candidate
        }
      }
    }
    vertices_changed += x

    //add *x into vertices as well
    for (it <- vertices_changed) {
      val x: VertexId = it

      val numEdges: Int = out.getNumEdges(x)
      val edges: Array[VertexId] = out.getEdges(x)
      val labels: Array[Byte] = out.getLabels(x)

      for (i <- 0 until numEdges) {
        if (grammar.isDereference(labels(i))) {
          vertices_changed += edges(i)
        }
        if (grammar.isDereference_reverse(labels(i))) {
          vertices_affected += edges(i)
        }
      }
    }
  }

  def isDeletable(src: VertexId, dst: VertexId, label: Byte, vertices_changed: mutable.Set[VertexId], vertices_affected: mutable.Set[VertexId], grammar: Grammar): Boolean = {
    //don't delete self-loop edges
    if(src == dst && grammar.isEruleLabel(label)){
      return false
    }

    //delete all the ('a', '-a', 'V', 'M', and other temp labels) edges associated with that within vertices_changed
    if(vertices_changed.contains(src) || vertices_changed.contains(dst)){
      return !grammar.isDereference_bidirect(label)
    }

    //delete all the ('V', 'M', and other temp labels) edges associated with that within vertices_affected
    //	else if(vertices_affected.find(src) != vertices_affected.end() || vertices_affected.find(dst) != vertices_affected.end()){
    //		return !grammar->isDereference_bidirect(label) && !grammar->isAssign_bidirect(label);
    //	}

    false
  }

  def findDeletedEdges(edgesToDelete: EdgeArray, src: VertexId, vertices_changed: mutable.Set[VertexId], vertices_affected: mutable.Set[VertexId], grammar: Grammar, deleted: EdgeArray) = {
    for (i <- 0 until edgesToDelete.getSize()) {
      val dst: VertexId = edgesToDelete.getEdges()(i)
      val label: Byte = edgesToDelete.getLabels()(i)
      if (isDeletable(src, dst, label, vertices_changed, vertices_affected, grammar)) {
        deleted.addOneEdge(dst, label)
      }
    }
  }
}
