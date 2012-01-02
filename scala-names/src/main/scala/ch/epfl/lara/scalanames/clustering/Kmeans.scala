package ch.epfl.lara.scalanames.clustering

import java.io.BufferedReader
import java.io.FileReader
import java.io.BufferedWriter
import java.io.FileWriter
import scala.collection.mutable.HashMap
import scala.util.Random


object Kmeans {
  
  /** ------------- ARGUMENTS ------------- **/
  val libDataPath = "C:\\Documents and Settings\\Coubii\\workspace\\ScalaNames\\test\\libOutput.txt"
  val testDataPath = "C:\\Documents and Settings\\Coubii\\workspace\\ScalaNames\\test\\testOutput.txt"
  val output = "C:\\Documents and Settings\\Coubii\\workspace\\ScalaNames\\test\\KmeanOutput.txt"
  var dataPathToUse = testDataPath				//Where to find the good file
  var printy = false 							//If true, print out in output file
  var cluster = 10 								//Number of wanted clusters
  var endAfterXStep = 100						//Exit the algorithm after X step
  var threshold:Double = 0.225					//Threshold of OptBoolCluster
  //var emptyCluster : Boolean = false			//Use of the modified K-means for avoiding empty cluster
  var loop : Boolean = false					//Empty cluster do not make algorithm looping
  var analysis = false							//If the output should be for the analysis plugin

  /** ------------- ALGORITHM GLOBAL VARIABLES ------------ **/
  var dimSize = 0 								//The dimension size of observations 
  var observations : Double = 0 				//Cardinality of the observation set
  val data = new HashMap[String,List[Int]]		//The observation set retrieved from the input file
  var clusteredData = new HashMap[String,Int] 	//The observation set classified by it's cluster
  var endingCluster = new HashMap[String,Int]	//The result of the k_means algorithm
  var cs : List[DoubleCluster] = List()			//The list of all clusters
  var bs : List[OptBoolCluster] = List()		//The list of clusters used for final step
  var features = new HashMap[Int,String]		//List of feature for usefull output
  
  /** ------------ MISC ------------ **/
  lazy val out = new BufferedWriter(new FileWriter(output, false))
  val rand = new Random

  //Initialize list of DoubleCluster of size nb
  def buildClusters(nb:Int):List[DoubleCluster]= nb match {
    case 0 => List()
    case i => new DoubleCluster(i)::buildClusters(i-1)
  }
  //Initialize list of OptBoolCluster of size equals to cs.size = cluster
  def buildOptionClusters:List[OptBoolCluster] = {  
    def createOptBoolClusters(centroids: List[DoubleCluster]):List[OptBoolCluster] = centroids match {
      case Nil => List()
      case y :: ys => {
        val b = new OptBoolCluster(y.id,threshold)
        b.setPosFromDouble(y.getPos,y.getSize)
        b::createOptBoolClusters(ys)
      }
    }   
    createOptBoolClusters(cs) 
  }
  
  def buildData(path:String):Unit = {
       
    val buffer = new BufferedReader(new FileReader(path));
    var featureDef = false
    
	def read() : Unit = buffer.readLine() match { //Read a line
	  case null =>
	  case "{" => featureDef = true; read
	  case "}" => featureDef = false; read
	  case str if(featureDef)=> {
	    val f = str.split("\\u003B") //;
	    if(f.length==2){
	      features.put(f.head.toInt,f.tail.head)	      
	    } else throw new Exception("feature definition is not right: "+str)
	    read
	  }
	  case str => { 
	    val entry : List[String] = str.split("\\s").toList //Split it on blank characters      
	    data.put(entry.last,convert(entry.-(entry.last)))  //Store it into a database
	    read
	}}
	def convert(ls: List[String]): List[Int] = ls.map(_.toInt)
	
	try{
	  read
	}catch{
      case e => println("Unable to retrieve data: "+e.toString); System.exit(0)
    }	
	if(data.isEmpty) {println("data is empty, system will exit."); System.exit(0)}
  }
  
  //Assign to every element a cluster at random
  def randomPartition:Unit = data.foreach(d => clusteredData.put(d._1,rand.nextInt(cluster)+1))
  
  def assignment(cs:List[Cluster[_]]):Unit = {
    
    def assignClosestCluster(elem: (String,List[Int])):Unit = {
      var minMean = Double.MaxValue
      var clusterNumber = -1
      
      for(cluster <- cs){
        val distance = cluster.distanceFrom(elem._2)
        if (minMean > distance) {
          clusterNumber = cluster.id
          minMean = distance
        }
      }
      clusteredData.put(elem._1,clusterNumber)
    }
    
    clusteredData = new HashMap[String,Int] //empty the clusteredData
    data.foreach(assignClosestCluster) //assign data to correct cluster   
  }
  
  def update(cs:List[Cluster[_]]):Boolean = {
    
    val csCopy = cs.map(_.copy) //Copy of the cluster before the update
    var centers = new HashMap[Int,List[Double]]
    var clusterSize = new Array[Int](cluster)
    
    def init():Unit = {
      //Initialize a list of double of size dimSize
      def newD(dimSize:Int):List[Double]= dimSize match {
      	case 0 => List()
      	case i => 0.0::newD(i-1)
      }
      //Put a new empty centroid into centers
      def addEmptyCluser(cluster: Int,doubleList: List[Double]):Unit = cluster match {
      	case 0 =>
      	case i => centers.put(i,doubleList); addEmptyCluser(i-1,doubleList)
      }     
      //Initiate all elements of clusterSize to 0
      var i = 0
      while(i<cluster){
    	clusterSize(i) = 0
    	i += 1
      }
      addEmptyCluser(cluster,newD(dimSize))
    }   
   
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
      }
    }  
    
     def divide(elem: (Int,List[Double])): Unit = {
       def kMeans(ls: List[Double]): List[Double] = ls match { // K-means
         case Nil => List()
         case x :: xs => if(clusterSize(elem._1-1)==0) x::kMeans(xs) else x/clusterSize(elem._1-1)::kMeans(xs)
       }
       centers.put(elem._1,kMeans(elem._2))     
    }
     
  /*  def m_kMeans():Unit = {
   *   
   *   def ++(ls1:List[Double],ls2:List[Double]):List[Double] = (ls1,ls2) match {
   *     case (Nil,Nil)=>Nil
   *     case (x::xs,Nil)=>x::xs
   *     case (x::xs,y::ys)=>(x+y):: ++(xs,ys)
   *   }
   *   for(i <- 1 to cluster) {
   *     clusterSize(i-1) +=1
   *     val exCenter = csCopy.apply(i-1).distWithList
   *     val temp = centers.apply(i)
   *     centers = centers-i
   *     centers.put(i,++(temp,exCenter))
   *   }     
   * }
   */ 
    //Initiate algorithm step
    init
    //For all observation, add their distance to their respective cluster
    clusteredData.foreach(addDistanceVector) 
    //If we run the modified kMean
    //if(emptyCluster) m_kMeans()
    //Divide by the cardinality of the number of observation
    centers.foreach(divide)       
    //Update the centroid
    cs.map(centroid => centroid.setPosFromDouble(centers.apply(centroid.id),clusterSize(centroid.id-1)))
    
    //If a centroid was updated, then return true
    //println("co="+csCopy)
    //println("cs="+cs)
    csCopy != cs
  }
  
  def optionStep:Unit = {
    bs = buildOptionClusters
    val formerBs = bs.map(_.copy)  
    
    assignment(bs)
    val mod : Boolean = update(bs)
    val moved : Int = endingCluster.filter(x => clusteredData.apply(x._1)!=x._2).size   
    val emptyC : Int = bs.filter(_.isEmpty).size
       
    def pick3atRandom(c: Cluster[_]) = {
      if(!c.isEmpty){
    	val mcs = clusteredData.filter(_._2==c.id).toIndexedSeq
    	val size = mcs.size
    	for (i <- 1 to 3) {
    	  import m.{_1 => name }
    	  val m = mcs.apply(rand.nextInt(size))
    	  println("method: "+name+" in cluster "+m._2+"\nSign: "+data.apply(name))
    }}}
    def haz(optBoolC: OptBoolCluster): Unit = {
      def inner(os:List[Option[Boolean]],id:Int):Unit = os match {
      	case Nil => 
      	case Some(true)::xs => println("Haz     "+features(id)); inner(xs,id+1)//Haz
      	case Some(false)::xs => println("Haz NOT "+features(id)); inner(xs,id+1) //Haz not
      	case None::xs => inner(xs,id+1)
      }
      if(!optBoolC.isEmpty) inner(optBoolC.getPos,1)
    }
    def prettyOutput(cs:List[DoubleCluster],fbs:List[OptBoolCluster],bs:List[OptBoolCluster]): Unit = (cs,fbs,bs) match {
      case (Nil,Nil,Nil) => println("# objects moved during the one step-check: "+moved+
    		  						"\nEmptyCluster: "+emptyC+
    		  						"\nClusters are"+(if(!mod)" not" else "")+" modified")
      case (x::xs,y::ys,z::zs) => {
        println(x+"\n"+y+"\n"+z)
        haz(z)
        pick3atRandom(z)
        prettyOutput(xs,ys,zs)
      }
      case _ => //something went wrong
    }
    
    prettyOutput(cs,formerBs,bs)
  }
  
  def analysisPhase = {
    def printCluster(c: OptBoolCluster):Unit = {
      out.write("<cl>\n")
      out.write(c.id+" "+c.getPos.map(_ match {case Some(false) => 0; case Some(true) => 1; case None => "?"}).mkString(" ")+"\n")
    }
    def printMethod(m: (String,Int)):Unit = {
      out.write("<m>\n")
      out.write(m._2+" "+m._1+"\n")
    }
    def printFeature(f: (Int,String)):Unit = {
      out.write("<f>\n")
      out.write(f._1+" "+f._2+"\n")
    }
    if (analysis){
      try{
        bs.foreach(printCluster)
        endingCluster.foreach(printMethod)
        features.foreach(printFeature)
        out.flush()
      } catch {
        case e => println("I/O error: "+e)
      }
    }
  }
  
    //TODO add args outputfile
  def checkArgs(args: Array[String]) = {
    //Print into output file or no | By default; false
    if (args.contains("-p")) printy = true
    //Choose number of clusters    | By default; 10
    if (args.contains("-cl")){
      val index = args.indexOf("-cl")
      if(args.length > index+1){
        try{
          val c = args.apply(index+1).toInt
          if(c <= 2) throw new Exception("Cluster number should be greater than 2: "+c)
          else if (c > 100) throw new Exception("Cluser number should be smaller than 101: "+c)
          else cluster = c
        } catch {
          case e => println("Correct number of clusters expected after -cl: "+e); System.exit(0)
    }}}
    //Exit algorithm after number of step | By default; 100
    if(args.contains("-exit")){
      val index = args.indexOf("-exit")
      if(args.length > index+1){
        try{
          val end = args.apply(index+1).toInt
          if(end < 1) throw new Exception("Algorithm should take at least 1 step")
          endAfterXStep = end
        } catch {
          case e => println("Correct number of steps expected after -exit: "+e); System.exit(0)
    }}}
    //Choose the threshold use for OptionCluster | By default 0.225
    if(args.contains("-th")){
      val index = args.indexOf("-th")
        if(args.length > index+1){
          try{
            val th = args.apply(index+1).toInt
            if(th < 0 || th > 0.5) throw new Exception("Threshold should be between 0 and 0.5: "+th)
            threshold = th
          } catch {
            case e => println("Correct threshold double number expected after -th: "+e); System.exit(0)
    }}}
    //Choose to run with the Library of the test file | By default; test
    if(args.contains("-t")) dataPathToUse = testDataPath
    else if(args.contains("-lib")) dataPathToUse = libDataPath   
    //Choose k-mean or it's modified version | By default; false
    //if(args.contains("-noEmpty")) emptyCluster = true
    //Choose if an empty cluster make loop the algorithm | by default; false
    if(args.contains("-noLoop")) loop = true
    if(args.contains("-analysis")) analysis = true
  }
  
  //TODO ajouter feature
  //TODO aller voir dans le code 
  
  def main(args: Array[String]) = {

    checkArgs(args)
    //Retrieve data sample from input file or exit
    buildData(dataPathToUse)

    //Build the centroids
    dimSize = data.first._2.length
    observations = data.elements.length
    cs = buildClusters(cluster).reverse

    //Assign to every elements a cluster at random
    randomPartition
    
    //Run the algorithm
    var cs2stepAgo : List[DoubleCluster] = Nil
    var cs1stepAgo : List[DoubleCluster] = Nil
    var i =0
    while(if(i<endAfterXStep) update(cs) else false){
       if(cs==cs2stepAgo){ //reach stable point
         println("fixed point reached at round "+i)
         i=endAfterXStep 
       } else {
    	cs2stepAgo = cs1stepAgo.map(_.copy)
    	cs1stepAgo = cs.map(_.copy)
        assignment(cs)
    	println("round :"+i+"\t"+cs)
    	i+=1
       }
    }
    //Copy 
    endingCluster ++= clusteredData
    //printMyStuff
    //Transform DoubleCluster to OptBoolCluster and perform one step
    optionStep
    analysisPhase
    
  }
}

