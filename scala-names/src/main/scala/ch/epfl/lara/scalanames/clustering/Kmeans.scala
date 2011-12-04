package ch.epfl.lara.scalanames.clustering

import java.io.BufferedReader
import java.io.FileReader
import scala.collection.mutable.HashMap
import scala.util.Random
import java.io.BufferedWriter
import java.io.FileWriter

object Kmeans {
  
  val libDataPath = "C:\\Documents and Settings\\Coubii\\workspace\\ScalaNames\\test\\libOutput.txt"
  val testDataPath = "C:\\Documents and Settings\\Coubii\\workspace\\ScalaNames\\test\\testOutput.txt"
   
  var cluster = 10 							//Number of wanted clusters
  var dimSize = 0 							//The dimension size of observations 
  var observations : Double = 0 			//Cardinality of the observation set
  val data = new HashMap[String,List[Int]]	//The observation set retrieved from the input file
  
  var clusteredData = new HashMap[String,Int] //The observation set classified by it's cluster
  var cs : List[Centroid] = List()			  //The list of all clusters
  
  val newCentroid = new HashMap[Int,List[Double]] //Val used for optimization of update phase
  
  val output: String = ".\\KmeanOutput.txt"
  lazy val out = new BufferedWriter(new FileWriter(output, false))

  
  //Initialize the centroids 
  def buildCentroid(nb:Int,size:Int):List[Centroid]= nb match {
    case 0 => List()
    case i => new Centroid(i,size)::buildCentroid(i-1,size)
  }
  
  def buildData(path:String):Unit = {
    
    val buffer = new BufferedReader(new FileReader(path));

	def read : Unit = buffer.readLine() match {
	  case null =>
	  case str => { 
	    //println(str)
	    val entry : List[String] = split(str) 
	    data.put(entry.head,convert(entry.tail)) 
	    read
	  } 
	}
	def split(str: String): List[String] = str.split("\\s").toList
	def convert(ls: List[String]): List[Int] = ls.map(_.toInt)
	
	read
	
  }
  
  //Assign to every element a cluster at random
  def randomPartition:Unit ={
    val rand = new Random
    for(d <- data.keySet){
      clusteredData.put(d,rand.nextInt(cluster)+1)
    }
  }
  
  def assignement():Unit = {
    
    def assignClosestCluster(elem: (String,List[Int])):Unit = {
      var minMean = Double.MaxValue
      var clusterNumber = -1
      
      for(cluster <- cs){
        val distance = cluster.distanceFrom(elem._2)
        if (minMean > distance) {
          clusterNumber = cluster.i
          minMean = distance
        }
      }
      clusteredData.put(elem._1,clusterNumber)
    }
    
    clusteredData = new HashMap[String,Int] //empty the clusteredData
    data.foreach(assignClosestCluster) //assign data to correct cluster   
  }
  
  def update():Boolean = {
    
    val csCopy : List[Centroid] = cs.map(_.copy) //Copy of the cluster before the update
    var clusterSize: Array[Int] = new Array[Int](cluster)
    
    //Initialize a list of double of size dimSize
    def newD(dimSize:Int):List[Double]= dimSize match {
      case 0 => List()
      case i => 0.0::newD(i-1)
    }
    //Initialize the newCentroid variable
    def addEmptyCluser(cluster: Int,doubleList: List[Double]):Unit = cluster match {
      case 0 =>
      case i => newCentroid.put(i,doubleList); addEmptyCluser(i-1,doubleList)
    }
      
    //Initiate once the empty List
    if(newCentroid.isEmpty) addEmptyCluser(cluster,newD(dimSize))
    //A copy of the empty centroid 
    val centers : HashMap[Int,List[Double]] = newCentroid.clone()
    
    def sum(ls1:List[Int],ls2:List[Double]): List[Double] = ls1 match {
      case Nil => List()
      case x :: xs => x+ls2.head::sum(xs,ls2.tail)
    }    
    def addDistanceVector(elem: (String,Int)):Unit = elem._2 match {
      case x => {
        val actualClusterVector = centers.apply(x)
        val elementVector = data.apply(elem._1)
        centers.put(x,sum(elementVector,actualClusterVector))
        clusterSize(x-1) += 1
        //faire la somme des elements par cluster: tableau[int] of size cs.length
      }
    }   
     def divide(elem: (Int,List[Double])): Unit = {
      def apply(ls: List[Double]): List[Double] = ls match {
        case Nil => List()
        case x :: xs => if(clusterSize(elem._1-1)==0) x::apply(xs) else x/clusterSize(elem._1-1)::apply(xs)
      }
      centers.put(elem._1,apply(elem._2))
      
    }
     var i = 0
    while(i<10){
      clusterSize(i) = 0
      i += 1
    }
    //For all observation, add their distance to their respective cluster
    clusteredData.foreach(addDistanceVector) 
    //Divide by the cardinality of the number of observation
    centers.foreach(divide)
    //Update the centroid
    for (centroid <- cs) centroid.updatePos(centers.apply(centroid.i))
    
    //If a centroid was updated, then return true
    //println("cs:"+cs+"\ncsCopy:"+csCopy+"\nResult:"+(csCopy != cs))
    csCopy != cs
  }
  
  //TODO add args for specifying nb cluster, inputfile and if print or not
  def main(args: Array[String]) = {

    //Retrieve data sample from input file or exit
    try{
      buildData(libDataPath)
    }catch{
      case e => println("Unable to retrieve data: "+e.toString); System.exit(0)
    }

    //Build the centroids
    if(data.isEmpty) {
      println("data is empty, system will exit."); System.exit(0)
    } else {
    	dimSize = data.first._2.length
    	observations = data.elements.length
    	cs = buildCentroid(cluster,dimSize)

    	//Assign to every elements a cluster at random
    	randomPartition
    
    	//Run the algorithm
    	//TODO add possibility to exit after X steps
    	//If step i = step i+2 then exit
    	var i =0
    	while(update()){
    		println("round :"+i)
    		if(i>=0){
    		  println(cs)
    		}
    		assignement()
    		i=i+1
    	}
    
    	//print result
    	println("---------RESULTS--------")
    	clusteredData.foreach(elem =>
    	  {
    	    val str = elem._2 +"\t"+elem._1+"\n"
    		try{
    		  out.write(str)
    		  print(str)
    		  
    		} catch {
    		  case e => println("error during writing: "+str)
    		}
    	  })
    		
    }
  }
}

