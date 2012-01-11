package ch.epfl.lara.scalanames.correlation
import ch.epfl.lara.scalanames.clustering.Kmeans
import java.io.BufferedWriter
import java.io.FileWriter

object clusterTest {

    val size = 100
    val y = Math.floor(size.toDouble/2).toInt
    val yLow : Int = Math.floor(0.5*(size.toDouble)-0.98*Math.sqrt(size)).toInt
    val yHigh : Int = Math.ceil(0.5*(size.toDouble)+0.98*Math.sqrt(size)).toInt
    val cNb : List[Int] = /*List(10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)*/List(10,12,14,16,18,20,22,24,26,28,30)
    val out = new BufferedWriter(new FileWriter("C:\\Documents and Settings\\Coubii\\workspace\\ScalaNames\\test\\clusterTest.txt",false))
    //val tss : List[Double] = List(0.025,0.05,0.075,0.1,0.125,0.15,0.175,0.2,0.225,0.25)
    //val out = new BufferedWriter(new FileWriter("C:\\Documents and Settings\\Coubii\\workspace\\ScalaNames\\test\\thresholdTest.txt",false))
    
    var ecs = new Array[Int](size)
    var ss = new Array[Int](size)
    var qms = new Array[Int](size)
    var ads = new Array[Double](size)
    var acs = new Array[Double](size)
    var adbs = new Array[Double](size)
  
  def main(args: Array[String]):Unit = {
  

   cNb.foreach(cn => {
      for(i <- 0 to size-1){
        val kMeans = Kmeans
        kMeans.apply(List("-cl",cn.toString(),"-lib","-noLoop"))
        ecs.update(i,kMeans.emptyCluster)
        ss.update(i,kMeans.switched)
        qms.update(i,kMeans.?#)
        ads.update(i,kMeans.averageDist)
        acs.update(i,kMeans.averageClusterSize)
        adbs.update(i,kMeans.averageDistBefore)
        println("###################### iter "+i+" for clusterSize "+cn+" done ##################")
      }
      sort
      prettyPrint(cn)
   })
      
  /*  tss.foreach(ts => {
      for(i <- 0 to size-1){
        val kMeans = Kmeans
        kMeans.apply(List("-th",ts.toString(),"-lib","-noLoop"))
        ecs.update(i,kMeans.emptyCluster)
        ss.update(i,kMeans.switched)
        qms.update(i,kMeans.?#)
        ads.update(i,kMeans.averageDist)
        acs.update(i,kMeans.averageClusterSize)
        println("###################### iter "+i+" for threshold "+ts+" done ##################")
      }
      sort
      prettyPrint(ts)
    })*/
  }
  
  
  def sort:Unit = {
    ecs = ecs.sorted
    ss = ss.sorted
    qms = qms.sorted
    ads = ads.sorted
    acs = acs.sorted
  }
  
  
  
  def prettyPrint(cn:Double):Unit = {
    val str = cn+" "+ecs(y)+" "+ecs(yLow)+" "+ecs(yHigh)+" "+ss(y)+" "+ss(yLow)+" "+ss(yHigh)+" "+qms(y)+" "+qms(yLow)+" "+qms(yHigh)+" "+ads(y)+" "+ads(yLow)+" "+ads(yHigh)+" "+acs(y)+" "+acs(yLow)+" "+acs(yHigh)+" "+adbs(y)+" "+adbs(yLow)+" "+adbs(yHigh)+"\n"
    out.write(str)
    out.flush
    print(str)
  }
}