package clustering

class Centroid(i:Int,size:Int) {
    private var pos : List[Double] = init(size)
	override def toString = "cluster "+i+" at position "+pos
	
	private def init(size:Int):List[Double] = size match {
      case 0 => List()
      case x => List(0.0):::init(size-1)
    }
    
    override def equals(that: Any):Boolean = that match {
      case c:Centroid if(c.j==i)=> for(ps <- getPos.zip(c.getPos)){if(ps._1!=ps._2)return false};true
      case _ => false
    }
	  
	def updatePos(ps:List[Double]):Unit = pos = ps
	
	def getPos:List[Double] = pos
	
	def j=i
	
	def distanceFrom(ls:List[Int]):Double = {	  
	  def apply(ls:List[Int],pos:List[Double]): Double = ls match {
	    case Nil => 0
	    case x :: xs => (x-pos.head).abs + apply(xs,pos.tail)
	  }	  
	  apply(ls,pos)
	}
	
  
  
}