import org.apache.spark.graphx.VertexId

//之后可考虑使用单例模式
//inline可能需要调节scala编译参数
class Singleton (set: Set[VertexId]) extends Serializable
{
  private val singletonSet = set

  @inline final def isSingleton(vertex_id: VertexId) : Boolean = singletonSet.contains(vertex_id)

}
