package ch.epfl.lara.scalanames.clustering

import scala.collection.immutable.List

class DoubleCluster(val id:Int) extends Cluster[Double] {

    private var pos : List[Double] = List()
	override def toString = "Double cluster "+id+" at position ["+pos.map(_.toString()).mkString(",")+"]"
    
  def copy(): DoubleCluster = {
    val cp = new DoubleCluster(id)
    cp.updatePos(this.pos)
    cp
  }

  def distanceFrom(ls: List[Int]): Double = distance(ls.map(_.toDouble))
  
  def distanceFrom2(ls: List[Double]): Double = distance(ls)
  
  def distance(ls: List[Double]): Double = {
     def inner(zhis: List[Double],that: List[Double]): Double = (zhis,that) match {
      case (Nil,Nil)=> 0
      case (x::xs,y::ys)=>  (x-y).abs+inner(xs,ys)
    }   
    inner(getPos(),ls)
  }
  
  def distFromIndex(index:Int,x:Double):Double = if(pos.isEmpty) 0 else (x-pos.apply(index)).abs

  def getPos(): List[Double] = { pos }

  override def equals(that: Any):Boolean = (that != null) && (that match {
      case d:DoubleCluster if(d.id == id)=> this.getPos()==d.getPos()   
      case _ => false
    })

  def updatePos(ps: List[Double]): Unit = { pos = ps }
  
  def setPosFromDouble(ps: List[Double]): Unit = updatePos(ps)

}