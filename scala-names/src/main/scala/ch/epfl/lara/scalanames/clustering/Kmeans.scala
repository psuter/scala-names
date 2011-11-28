package ch.epfl.lara.scalanames.clustering

import java.io.BufferedReader
import java.io.FileReader
import scala.collection.mutable.HashMap
import scala.util.Random

object Kmeans {
  
  val dataPath = "C:\\Documents and Settings\\Coubii\\workspace\\ScalaNames\\test\\output.txt"
   
  var cluster = 3 							//Number of wanted clusters
  var dimSize = 0 							//The dimension size of observations 
  var observations : Double = 0 			//Cardinality of the observation set
  val data = new HashMap[String,List[Int]]	//The observation set retrieved from the input file
  
  val clusteredData = new HashMap[String,Int] //The observation set classified by it's cluster
  val cs : List[Centroid] = List()			  //The list of all clusters
  
  val newCentroid = new HashMap[Int,List[Double]] //Val used for optimization of update phase
  
  def buildCentroid(nb:Int,size:Int):List[Centroid]= nb match {
    case 0 => List()
    case i => new Centroid(i,size)::buildCentroid(i-1,size)
  }
  
  def buildData:Unit = {
    
    val buffer = new BufferedReader(new FileReader(dataPath));

	def apply : Unit = buffer.readLine() match {
	  case null =>
	  case str => { 
	    //println(str)
	    val entry : List[String] = split(str) 
	    data.put(entry.head,convert(entry.tail)) 
	    apply
	  } 
	}
	def split(str: String): List[String] = str.split("\\s").toList
	def convert(ls: List[String]): List[Int] = ls.map(s=> s.toInt)
	
	apply
	
  }
  
  //Assign to every element a cluster at random
  def randomPartition:Unit ={
    val rand = new Random
    for(d <- data.keySet){
      clusteredData.put(d,rand.nextInt(3)+1)
    }
  }
  
  def assignement:Unit = {
    
    def assignClosestCluster(elem: (String,List[Int])):Unit = {
      var minMean = Double.MaxValue
      var clusterNumber = -1
      
      for(cluster <- cs){
        val distance = cluster.distanceFrom(elem._2)
        if (minMean > distance) {
          clusterNumber = cluster.j
          minMean = distance
        }
      }
     // clusteredData.remove(elem._1)
      clusteredData.put(elem._1,clusterNumber)
    }
    clusteredData.foreach(f => clusteredData.remove(f._1)) //empty the clusteredData
    data.foreach(assignClosestCluster) //assign data to correct cluster
    
  }
  
  def update:Boolean = {
    
    val csCopy = cs //Copy of the cluster before the update
    
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
    val centers = newCentroid 
    
    def sum(ls1:List[Int],ls2:List[Double]): List[Double] = ls1 match {
      case Nil => List()
      case x :: xs => x+ls2.head::sum(xs,ls2.tail)
    }    
    def addDistanceVector(elem: (String,Int)):Unit = elem._2 match {
      case x => {
        val actualClusterVector = centers.apply(x)
        val elementVector = data.apply(elem._1)
        centers.put(x,sum(elementVector,actualClusterVector))
      }
    }   
     def divide(elem: (Int,List[Double])): Unit = {
      def apply(ls: List[Double]): List[Double] = ls match {
        case Nil => List()
        case x :: xs => x/observations::apply(xs)
      }
      centers.put(elem._1,apply(elem._2))
    }
    
    //For all observation, add their distance to their respective cluster
    clusteredData.foreach(addDistanceVector) 
    //Divide by the cardinality of the number of observation
    centers.foreach(divide)
    //Update the centroid
    for (centroid <- cs) centroid.updatePos(centers.apply(centroid.j))  
    //If a centroid was updated, then return true
    for(css <- csCopy.zip(cs)){if(!css._1.equals(css._2))return true};false
  }
  
  //TODO add args for specifying nb cluster, inputfile and nb steps
  def main(args: Array[String]) = {

    //Retrieve data sample from input file or exit
    try{
      buildData
    }catch{
      case e => println("Unable to retrieve data: "+e.toString); System.exit(0)
    }

    //Build the centroids
    if(data.isEmpty) {
      println("data is empty, system will exit."); System.exit(0)
    } else {
    	dimSize = data.first._2.length
    	observations = data.elements.length
    	cs::buildCentroid(cluster,dimSize)

    	//Assign to every elements a cluster at random
    	randomPartition
    
    	//Run the algorithm
    	//TODO add possibility to exit after X steps
    	var i =0
    	while(update){
    		println("round :"+i)
    		assignement
    		i=i+1
    	}
    
    	//print result
    	for(elem <- clusteredData.elements){
    		println(elem._2 +"\t"+elem._1)
    	}
  }
  }
}

