package ch.epfl.lara.scalanames.clustering

import scala.collection.mutable.HashMap
import scala.util.Random
import java.io.BufferedReader
import java.io.InputStreamReader

object LibCluster {
  
  private val cs : HashMap[Int,OptBoolCluster] = new HashMap[Int,OptBoolCluster]
  private val dataC : HashMap[String,Int] = new HashMap[String,Int]
  private val dataM = new HashMap[String,List[Int]]
  private val features : HashMap[Int,String] = new HashMap[Int,String] 
  
  private var applied : Boolean = false

  def apply():Unit = {
    val in = new BufferedReader(new InputStreamReader(getClass().getResourceAsStream("/src/main/resources/KmeanOutput.txt")))
    //lazy val lines = scala.io.Source.fromFile("/KmeanOutput.txt").getLines
    //val in = new BufferedReader(new FileReader("C:\\Documents and Settings\\Coubii\\workspace\\ScalaNames\\test\\KmeanOutput.txt"))
     
    def cluster(strs: List[String]):List[Option[Boolean]] = strs match {
      case Nil => Nil
      case "1" :: xs => Some(true)::cluster(xs)
      case "0" :: xs => Some(false)::cluster(xs)
      case "?" :: xs => None::cluster(xs)
      case e => throw new Exception("Expected 0,1 or ?. Found: "+e)
    }
    
    def clID(next: String) = { 
      try {
        val line = next.split("\\s").toList
        val id = line.head.toInt
        val values = cluster(line.tail)
        val obc = new OptBoolCluster(id.toInt,0)
        obc.updatePos(values,-1)
        cs.put(obc.id,obc)
      } catch {
        case e => println("Expected int list OptBoolean: "+e)
    }}
    
    def mID(next:String) = {
      try{
        val line = next
        val sp= line.split("\\s").toList
        val name = sp.tail.head
        val signs = sp.tail.tail
        val c = sp.head
        dataM.put(name,signs.map(_.toInt))
        dataC.put(name,c.toInt)
      } catch {
        case e => println("Expected int string. Found: "+e)
    }}
    
    def fID(next:String) = {
      try{
        val line = next
        val str = {val sp=line.split("\\s").toList; (sp.head.toInt,sp.tail)}
        features.put(str._1,str._2.mkString(" "))
      } catch {
        case e => println("Expected Found: "+e)
    }}
        
    def read :Unit= in.readLine() match {
      case "<cl>" => clID(in.readLine()); read
      case "<m>" => mID(in.readLine()); read
      case "<f>" => fID(in.readLine()); read
      case "<end>" =>
      case _ =>
    }
    
    try{
      read
      //update the size of each cluster
      cs.foreach(c =>{
        import c.{_2 => currentCluster} 
        currentCluster.updatePos(currentCluster.getPos,dataC.filter(entry => entry._2==c._1).size)
      })
      applied = true
      //in.close()
    } catch {
      case e => println("I/O error: "+e)
  }}
 
  def checkFeatures(fs: List[(Int,String)]):Boolean = {
    if(!applied) apply
    
    fs match {
    	case Nil => true
    	case (x,y)::xs if features.apply(x)==y => checkFeatures(xs)
    	case _ => false	
    }
  }
  
  /**return the cluster with this id **/
  def cluster(id: Int): OptBoolCluster = {
    if(!applied) apply
    cs.apply(id)
  }
  
  /*def shoudHaveFs(id: Int): String = {
    
    def f(p : List[Option[Boolean]],i:Int):String = p match {
      case Nil => ""
      case Some(true) :: xs =>  "Should     have "+features(i+1)+".\n"+f(xs,i+1)
      case Some(false) :: xs => "Should NOT have "+features(i+1)+".\n"+f(xs,i+1)
      case None :: xs => f(xs,i+1)
    }
    f(cluster(id).getPos,0)
  }
  
  def shoudHaveF(id:Int, fId:Int): Option[Boolean] = {
    cluster(id).getPos.apply(fId-1) 
  }*/
  
  /**return 3 method from that cluster **/
  def take3AtRandom(id: Int): ((String,List[Int]),(String,List[Int]),(String,List[Int])) = {
    if(!applied) apply
    val filteredD = dataC.filter(ds => {ds._2 == id}).toIndexedSeq
    val rand = new Random
    val size = filteredD.size   
    def nextItem = {
      val fname = filteredD.apply(rand.nextInt(size))._1
      (fname, dataM(fname))
    }
    
    (nextItem,nextItem,nextItem)
  }
  
  def clusterHaz(id: Int):String = {
	def inner(ls: List[Option[Boolean]], fId: Int): String = ls match {
      case Nil => ""
      case Some(true) :: xs => "HAZ     "+features.apply(fId)+"\n"+inner(xs,fId+1)
      case Some(false) :: xs =>"HAZ NOT "+features.apply(fId)+"\n"+inner(xs,fId+1)
      case None :: xs => inner(xs,fId+1)
    }
	if(!applied) apply
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
    if(!applied) apply
    cs.apply(cId)
  }
  
  def nearestClusterWithDistance(pos: List[Int]):(OptBoolCluster,Double) = {
    if(!applied) apply
    val c = nearestCluster(pos)
    (c,c.distanceFrom(pos))
  }
  
  
}