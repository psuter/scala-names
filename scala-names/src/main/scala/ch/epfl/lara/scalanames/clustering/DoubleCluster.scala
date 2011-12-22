package ch.epfl.lara.scalanames.clustering

import scala.collection.immutable.List

class DoubleCluster(val id:Int) extends Cluster[Double] {

    private var pos : List[Double] = List()

    override def toString = "Double cluster "+id+
							(if(isEmpty) " is empty"
							else " contains "+size+" element(s) and is at position ["+pos.map(_.toString()).mkString(",")+"]")
    
  def copy(): DoubleCluster = {
    val cp = new DoubleCluster(id)
    cp.updatePos(this.pos,size)
    cp
  }
    
  def isEmpty = size==0

  def distanceFrom(ls: List[Int]): Double = distanceFrom2(ls.map(_.toDouble))
  
  def distanceFrom2(ls: List[Double]): Double = {
     def inner(zhis: List[Double],that: List[Double]): Double = (zhis,that) match {
      case (Nil,Nil)=> 0
      case (x::xs,y::ys)=>  (x-y).abs+inner(xs,ys)
    }   
    if(isEmpty) Double.MaxValue else inner(getPos,ls)
  }
  
  /*def distance(ls: List[Double]): Double = {
     def inner(zhis: List[Double],that: List[Double]): Double = (zhis,that) match {
      case (Nil,Nil)=> 0
      case (x::xs,y::ys)=>  (x-y).abs+inner(xs,ys)
    }   
    inner(getPos,ls)
  }*/
  
  def distWithList = getPos
  def getPos = pos 
  def getSize: Int = size

  override def equals(that: Any):Boolean = (that != null) && (that match {
      case d:DoubleCluster if(d.id == id)=> this.getPos==d.getPos   
      case _ => false
    })

  def updatePos(ps: List[Double],size:Int): Unit = { pos = ps; this.size=size }
  
  def setPosFromDouble(ps: List[Double],size:Int): Unit = updatePos(ps,size)

}