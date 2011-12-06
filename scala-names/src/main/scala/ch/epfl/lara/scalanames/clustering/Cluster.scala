package ch.epfl.lara.scalanames.clustering

abstract class Cluster[T] {
  
  /**cluster id**/
  val id:Int
  
  def copy : Cluster[T]
  
  /**calculate the distance from this cluster to an observation**/
  def distanceFrom(ls:List[Int]):Double
  
  /**calculate the distance from this cluster to the list (representing another cluster) provided**/ 
  def distance(ls:List[T]):Double
  
  /**return the pos variable**/
  def getPos : List[T]
  
  def equals(that:Any):Boolean
  
  /**Update variable pos **/
  def updatePos(ps:List[T]):Unit
  
  /**Update variable pos from a list of double**/
  def setPosFromDouble(ps: List[Double]):Unit
  
}