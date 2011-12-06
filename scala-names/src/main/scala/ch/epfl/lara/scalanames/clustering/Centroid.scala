package ch.epfl.lara.scalanames.clustering

class Centroid(val id:Int, length:Int) /*extends Cluster[Double]*/ {
    
    private var pos : List[Double] = List()
	override def toString = "cluster "+id+" at position "+pos
	
	def copy : Centroid = {
      val cp = new Centroid(id, length)
      cp.updatePos(this.pos)
      cp
    }
    
    override def equals(that: Any):Boolean = (that != null) && (that match {
      case c:Centroid if(c.id == id)=> getPos==c.getPos 
      case _ => false
    })
	  
	def updatePos(ps:List[Double]):Unit = pos = ps
	
	def getPos:List[Double] = pos
	
	def distanceFrom(ls:List[Int]):Double = {	  
	  def apply(ls:List[Int],pos:List[Double]): Double = ls match {
	    case Nil => 0
	    case x :: xs => (x-pos.head).abs + apply(xs,pos.tail)
	  }	  
	  apply(ls,pos)
	} 
  
}