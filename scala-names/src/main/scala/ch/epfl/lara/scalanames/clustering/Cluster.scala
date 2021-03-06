package ch.epfl.lara.scalanames.clustering

abstract class Cluster[T] {
  
  /**cluster id**/
  val id:Int
  
  /**indicate how many objects are in this cluster**/
  protected var size:Int=0
  
  def copy : Cluster[T]
  
  /**calculate the distance from this cluster to an observation**/
  def distanceFrom(ls:List[Int]):Double
  
  /**calculate the distance from this cluster to a list of double **/
  def distanceFrom2(ls:List[Double]):Double
  
  /**return the pos variable**/
  def getPos : List[T]
  
  /**return the cardinality of the cluster**/
  def getSize : Int
  
  def equals(that:Any):Boolean
  
  /**used with m_kMeans **/
  //def distWithList: List[Double]
  
  /**Update variable pos and the size of the cluster**/
  def updatePos(ps:List[T],size:Int):Unit
  
  /**Update variable pos from a list of double and the size**/
  def setPosFromDouble(ps: List[Double],size:Int):Unit
  
  def isEmpty:Boolean
  
}