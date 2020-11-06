import org.apache.spark.graphx.VertexId

import scala.reflect.ClassTag
import util.control.Breaks._

object myalgo {

  def swap[T](A: Array[T], k: Int, r: Int) = {
    val tmp = A(k)
    A(k) = A(r)
    A(r) = tmp
  }

  def split(A: Array[VertexId], B: Array[Byte], l: Int, r: Int): Int = {
    val mid = (l + r) / 2
    var k = l
    if (A(mid) < A(k)) k = mid
    if (A(r) < A(k)) k = r
    if(k != r){
      swap(A, k, r)
      swap(B, k, r)
    }
    if(mid != l && A(mid) < A(l)){
      swap(A, mid, l)
      swap(B, mid, l)
    }
    val val_v: VertexId = A(l)
    val val_c: Byte = B(l)

    var i = l
    for(j <- l+1 to r){
      if((A(j) < val_v) || (A(j) == val_v && B(j) < val_c)){
        i += 1
        swap(A, i, j)
        swap(B, i, j)
      }
    }
    swap(A, i, l)
    swap(B, i, l)
    i
  }

  def insertSort(A: Array[VertexId], B: Array[Byte], l: Int, r: Int) = {
    for(j <- l + 1 to r){
      val key_v: VertexId = A(j)
      val key_c: Byte = B(j)
      var i = j - 1
      while(i >= l && (key_v < A(i) || (key_v == A(i) && key_c < B(i)))){
        A(i + 1) = A(i)
        B(i + 1) = B(i)
        i -= 1
      }
      A(i + 1) = key_v
      B(i + 1) = key_c
    }
  }

  def quickSort(A: Array[VertexId], B: Array[Byte], l: Int, r: Int): Unit = {
    if(l < r){
      if(r - l + 1 <= 10) insertSort(A, B, l, r)
    }
    else{
      val i = split(A, B, l, r)
      quickSort(A, B, l, i - 1)
      quickSort(A, B, i + 1, r)
    }
  }


  //解决函数参数不可变问题
  def removeDuple(len: Int, dstA: Array[VertexId], dstB: Array[Byte], srclen: Int, srcA: Array[VertexId], srcB: Array[Byte]): Int = {
    var tmp = len
    if(srclen != 0){
      dstA(0) = srcA(0)
      dstB(0) = srcB(0)
//      Array.copy(srcA, 0, dstA, 0, srclen)
//      Array.copy(srcB, 0, dstB, 0, srclen)
      tmp = 1
      for(i <- 1 until srclen){
        breakable{
          if(srcA(i) == srcA(i-1) && srcB(i) == srcB(i-1)) break()
          else{
            dstA(tmp) = srcA(i)
            dstB(tmp) = srcB(i)
            tmp += 1
          }
        }
      }
      tmp
    }
    else{

      tmp = 0;
      tmp
    }
  }

  //解决参数是val的问题，与c++的处理略有不同
  def myrealloc[T: ClassTag](arr: Array[T], size: Int, Tosize: Int): Array[T] = {
    val tmpArr = new Array[T](Tosize)
    for(i <- 0 until size){
      tmpArr(i) = arr(i)
    }
    tmpArr
  }

  @inline final def myCompare(v1: VertexId, l1: Byte, v2: VertexId, l2: Byte): Long = {
    if (v1 == v2) l1 - l2
    else v1 - v2
  }

  def minusTwoArray(dstA: Array[VertexId], dstB: Array[Byte], len1: Int, A1: Array[VertexId], B1: Array[Byte], len2: Int, A2: Array[VertexId], B2: Array[Byte]): Int = {
    // (A1,B1),(A2,B2) is sorted
    var len: Int = 0;
    if(len1 != 0){
      if(len2 != 0){
        var p1: Int = 0
        var p2: Int = 0
        while(p1 < len1 && p2 < len2){
          val value: Long = myCompare(A1(p1),B1(p1),A2(p2),B2(p2))
          if(value > 0) {
            p2 += 1
          }
          else if(value < 0){
            dstA(len) = A1(p1)
            dstB(len) = B1(p1)
            p1 += 1
            len += 1
          }
          else{
            p1 += 1
            p2 += 1
          }
        }
        while(p1 < len1){
          dstA(len) = A1(p1)
          dstB(len) = B1(p1)
          p1 += 1
          len += 1
        }
      }
      else{
        len = len1
        Array.copy(A1, 0, dstA, 0, len)
        Array.copy(B1, 0, dstB, 0, len)
      }
    }
    len
  }

  def unionTwoArray(dstA: Array[VertexId], dstB: Array[Byte], len1: Int, A1: Array[VertexId], B1: Array[Byte], len2: Int, A2: Array[VertexId], B2: Array[Byte]): Int = {
    var len: Int = 0
    if(len1 != 0){
      if(len2 != 0){
        var p1 = 0
        var p2 = 0
        while(p1 < len1 && p2 < len2){
          val value: Long = myCompare(A1(p1),B1(p1),A2(p2),B2(p2))
          if(value > 0){
            //println("done!")
            dstA(len) = A2(p2)
            dstB(len) = B2(p2)
            p2 += 1
            len += 1
          }
          else if(value < 0){
            dstA(len) = A1(p1)
            dstB(len) = B1(p1)
            p1 += 1
            len += 1
          }
          else{
            dstA(len) = A1(p1)
            dstB(len) = B1(p1)
            p1 += 1
            p2 += 1
            len += 1
          }
        }
        while (p1 < len1) {
          dstA(len) = A1(p1)
          dstB(len) = B1(p1)
          p1 += 1
          len += 1
        }
        while (p2 < len2) {
          dstA(len) = A2(p2)
          dstB(len) = B2(p2)
          p2 += 1
          len += 1
        }

      }
      else{
        len = len1
        Array.copy(A1, 0, dstA, 0, len)
        Array.copy(B1, 0, dstB, 0, len)
      }
    }
    else{
      if(len2 != 0){
        len = len2
        Array.copy(A2, 0, dstA, 0, len)
        Array.copy(B2, 0, dstB, 0, len)
      }
    }
    len
  }
}
