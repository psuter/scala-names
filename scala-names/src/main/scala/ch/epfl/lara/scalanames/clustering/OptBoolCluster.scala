package ch.epfl.lara.scalanames.clustering

import scala.collection.immutable.List

class OptBoolCluster(val id:Int, treshold :Double) extends Cluster[Option[Boolean]] {
  
  private var pos : List[Option[Boolean]] = List()

  override def toString = "Option[Boolean] cluster "+id+" at pos ["+
		   pos.map(x => x match{ case Some(true)=>1;case Some(false)=>0;case None=>"?"}).mkString(",")+"]"
   

  def copy(): OptBoolCluster = { 
     val cp = new OptBoolCluster(id, treshold)
     cp.updatePos(this.pos)
     cp
  }
  
  def setPosFromDouble(ls:List[Double]): Unit = {
    
    def inner(ls:List[Double]): List[Option[Boolean]] = ls match {
      case Nil => List()
      case x::xs if(x<treshold)=> Some(false)::inner(xs)
      case x::xs if(x>1-treshold)=> Some(true)::inner(xs)
      case x::xs => None::inner(xs)
    }
    updatePos(inner(ls))
  }
   
  override def equals(that: Any):Boolean = (that != null) && (that match {
      case b:OptBoolCluster if(b.id == id)=> {       
        def inner(zhis: List[Option[Boolean]],that:List[Option[Boolean]]): Boolean = (zhis,that) match {
          case (Nil,Nil) => true
          case (None::xs,None::ys) => inner(xs,ys)
          case (Some(x)::xs,Some(y)::ys) if(x==y) => inner(xs,ys)
          case _ => false
        }
        inner(getPos,b.getPos)
      }      
      case _ => false
  })

  def distanceFrom(ls: List[Int]): Double = {
	  distance(ls.map(x=>x match {case 0=>Some(false);case 1=>Some(true); case _ => None}))
  }
  
  def distance(ls: List[Option[Boolean]]): Double = { 
    
    def dist(zhis:List[Option[Boolean]],that:List[Option[Boolean]]): Double = (zhis,that) match {
      case (Nil,Nil) => 0
      case (Some(true)::xs,Some(true)::ys) => dist(xs,ys)
      case (Some(true)::xs,Some(false)::ys) => 1+dist(xs,ys)
      case (Some(false)::xs,Some(true)::ys) => 1+dist(xs,ys)
      case (Some(false)::xs,Some(false)::ys) => dist(xs,ys)
      case (None::xs,y::ys) => dist(xs,ys)
      case (x::xs,None::ys) => dist(xs,ys)      
    }
    dist(getPos,ls)
  }

  def getPos: List[Option[Boolean]] = { pos }

  def updatePos(ps: List[Option[Boolean]]): Unit = { pos = ps }

}