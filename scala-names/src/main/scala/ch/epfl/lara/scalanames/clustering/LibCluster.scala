package ch.epfl.lara.scalanames.clustering
import java.io.BufferedReader
import java.io.FileReader
import scala.collection.mutable.HashMap
import scala.util.Random

object LibCluster {
  
  private val cs : HashMap[Int,OptBoolCluster] = new HashMap[Int,OptBoolCluster]
  private val data : HashMap[String,Int] = new HashMap[String,Int]
  private val features : HashMap[Int,String] = new HashMap[Int,String] 

  def apply():Unit = {
    val in = new BufferedReader(new FileReader("C:\\Documents and Settings\\Coubii\\workspace\\ScalaNames\\test\\KmeanOutput.txt"))
     
    def cluster(strs: List[String]):List[Option[Boolean]] = strs match {
      case Nil => Nil
      case "1" :: xs => Some(true)::cluster(xs)
      case "0" :: xs => Some(false)::cluster(xs)
      case "?" :: xs => None::cluster(xs)
      case e => throw new Exception("Expected 0,1 or ?. Found: "+e)
    }
    
    def clID = { 
      try {
        val line = in.readLine().split("\\s").toList
        val id = line.head.toInt
        val values = cluster(line.tail)
        val obc = new OptBoolCluster(id.toInt,0)
        obc.updatePos(values,-1)
        cs.put(obc.id,obc)
      } catch {
        case e => println("Expected int list OptBoolean: "+e)
    }}
    
    def mID = {
      try{
        val line = in.readLine()
        val str = {val sp= line.split("\\s").toList; (sp.head.toInt,sp.last)}
        data.put(str._2,str._1)
      } catch {
        case e => println("Expected int string. Found: "+e)
    }}
    
    def fID = {
      try{
        val line = in.readLine()
        val str = {val sp=line.split("\\s").toList; (sp.head.toInt,sp.last)}
        features.put(str._1,str._2)
      } catch {
        case e => println("Expected Found: "+e)
    }}
        
    def read :Unit = in.readLine() match {
      case "<cl>" => clID; read
      case "<m>" => mID; read
      case "<f>" => fID; read
      case _ => Nil
    }
    try{
      read
      //update the size of each cluster
      cs.foreach(c =>{
        import c.{_2 => currentCluster} 
        currentCluster.updatePos(currentCluster.getPos,data.filter(entry => entry._2==c._1).size)
      })
    } catch {
      case e => println("error: "+e)
  }}
 
  def checkFeatures(fs: List[(Int,String)]):Boolean = fs match {
    case Nil => true
    case (x,y)::xs if features.apply(x)==y => checkFeatures(xs)
    case _ => false
  }
  
  /**return the cluster with this id **/
  def cluster(id: Int): OptBoolCluster = cs.apply(id)
  
  /**return 3 method from that cluster **/
  def take3AtRandom(id: Int): (String,String,String) = {
    val filteredD = data.filter(ds => {ds._2 == id}).toIndexedSeq
    val rand = new Random
    val size = filteredD.size   
    var nextItem = filteredD.apply(rand.nextInt(size))._1
    
    (nextItem,nextItem,nextItem)
  }
  
  def clusterHaz(id: Int):String = {
	def inner(ls: List[Option[Boolean]], fId: Int): String = ls match {
      case Nil => ""
      case Some(true) :: xs => "HAZ     "+features.apply(fId)+"\n"+inner(xs,fId+1)
      case Some(false) :: xs =>"HAZ NOT "+features.apply(fId)+"\n"+inner(xs,fId+1)
      case None :: xs => inner(xs,fId+1)
    }
    inner(cluster(id).getPos,1)
  }
  
  def nearestCluster(pos: List[Int]):OptBoolCluster = {
    var d = Double.MaxValue
    var cId = 0
    for (c <- cs){
      val currentD = c._2.distanceFrom(pos)
      if(currentD<d){
        d = currentD
        cId = c._2.id
    }}
    cs.apply(cId)
  }
  
  def nearestClusterWithDistance(pos: List[Int]):(OptBoolCluster,Double) = {
    val c = nearestCluster(pos)
    (c,c.distanceFrom(pos))
  }
  
  
  
}