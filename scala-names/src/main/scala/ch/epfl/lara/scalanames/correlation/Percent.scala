package ch.epfl.lara.scalanames.correlation
import java.io.BufferedReader
import java.io.FileReader
import scala.collection.mutable.HashMap

object Percent {
  
  var features = new HashMap[Int,String]		//List of feature for usefull output
  val data = new HashMap[String,List[Int]]		//The observation set retrieved from the input file
  var count: Array[Double] = null


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
  
  
  def main(args:Array[String]):Unit = {
    
    buildData("C:\\Documents and Settings\\Coubii\\workspace\\ScalaNames\\test\\libOutput.txt")
    count = new Array[Double](features.size)
    	
    data.foreach(m => f_count(m._2))
    val size = data.size
    count = count.map(x => x/size)
    features./*toList.sort((f1,f2)=> count(f1._1-1)>count(f2._1-1)).*/foreach(f => println(count(f._1-1)+" "+f._2))
  }
  
  def f_count(fs:List[Int]):Unit = {
        def inner(i:Int,fs: List[Int]):Unit = fs match {
          case Nil =>
          case 1 :: xs => count.update(i,count(i)+1); inner(i+1,xs)
          case 0 :: xs => inner(i+1,xs)
        }
        inner(0,fs)
  }
}