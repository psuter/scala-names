package ch.epfl.lara.scalanames.clustering

class Centroid(val i:Int, size:Int) {
    private var pos : List[Double] = init(size)
	override def toString = "cluster "+i+" at position "+pos
	
	def copy : Centroid = {
      val cp = new Centroid(i, size)
      cp.updatePos(this.pos)
      cp
    }
    
	private def init(size:Int):List[Double] = size match {
      case 0 => List()
      case x => List(0.0):::init(size-1)
    }
    
    override def equals(that: Any):Boolean = (that != null) && (that match {
      case c:Centroid if(c.i == i)=> {
        var e = true
        for(ps <- getPos.zip(c.getPos)){
        	if(ps._1!=ps._2)e = false
        }
        //println(this +" ==?== "+c +" is "+e)
        e
      }      
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