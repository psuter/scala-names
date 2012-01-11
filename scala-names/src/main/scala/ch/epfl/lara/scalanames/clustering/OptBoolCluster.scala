package ch.epfl.lara.scalanames.clustering

import scala.collection.immutable.List

class OptBoolCluster(val id:Int, treshold :Double) extends Cluster[Option[Boolean]] {
    
  private var pos : List[Option[Boolean]] = List()
  
  /**Indicate the number of ? in that cluster **/
  private var #? : Int = 0
  
  private var dim : Int = 0

  override def toString = "Option[Boolean] cluster "+id+ 
    					  (if(isEmpty) " is empty"
	  					  else " contains "+size+" element(s) and is at pos ["+posToString+"]")
   
  def posToString():String = getPos.map(x => x match{case Some(true)=>1;case Some(false)=>0;case None=>"?"}).mkString(" ") 


  def copy(): OptBoolCluster = { 
     val cp = new OptBoolCluster(id, treshold)
     cp.updatePos(this.pos,size)
     cp
  }
  
  def isEmpty = size==0
  
  def isCompletelyUndefined : Boolean = size == #?
  
  def setPosFromDouble(ls:List[Double],size:Int): Unit = {
    
    def inner(ls:List[Double]): List[Option[Boolean]] = ls match {
      case Nil => List()
      case x::xs if(x<treshold)=> Some(false)::inner(xs)
      case x::xs if(x>1-treshold)=> Some(true)::inner(xs)
      case x::xs => None::inner(xs)
    }
    updatePos(inner(ls),size)
  }
   
  override def equals(that: Any):Boolean = (that != null) && (that match {
      case b:OptBoolCluster if(b.id == id)=> getPos==b.getPos
      case _ => false
  })

  def distanceFrom(ls: List[Int]): Double = distanceFrom2(ls.map(_.toDouble))
  
  def distanceFrom2(ls: List[Double]): Double = {    
    def dist(zhis: List[Option[Boolean]],ls: List[Double]): Double = (zhis,ls) match {
      case (Nil,Nil) => 0
      case (Some(true)::xs,y::ys)=> 1-y+dist(xs,ys)
      case (Some(false)::xs,y::ys) => y+dist(xs,ys)
      case (None::xs,y::ys) => dist(xs,ys)
    }
    if(isEmpty || isCompletelyUndefined) Double.MaxValue else ((dim * dist(getPos,ls))/(dim - #?))
  }
  
  /*def distance(ls: List[Option[Boolean]]): Double = {   
    def dist(zhis:List[Option[Boolean]],that:List[Option[Boolean]]): Double = (zhis,that) match {
      case (Nil,Nil) => 0
      case (Some(true)::xs,Some(true)::ys) => dist(xs,ys)
      case (Some(true)::xs,Some(false)::ys) => 1+dist(xs,ys)
      case (Some(false)::xs,Some(true)::ys) => 1+dist(xs,ys)
      case (Some(false)::xs,Some(false)::ys) => dist(xs,ys)
      case (None::xs,y::ys) => dist(xs,ys)
      case (x::xs,None::ys) => dist(xs,ys)      
    }
  if(isEmpty || isCompletelyUndefined) Double.MaxValue else ((size * dist(getPos,ls))/(size - #?)) //this line is wrong

  }*/
  
  def distWithList = getPos.map(_ match{case Some(true)=>1.0;case _ =>0.0})

  def getPos = pos 
  
  def getSize = size
  
  def undefined = #?
  
  def updatePos(ps: List[Option[Boolean]], size:Int): Unit = { 
    pos = ps
    this.size = size
    #? = 0
    dim = ps.length
    ps.map(_ match {case None=> #? += 1 ;case _ =>})
    }


}