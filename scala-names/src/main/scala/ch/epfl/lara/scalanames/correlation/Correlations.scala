package ch.epfl.lara.scalanames.correlation

//@author Philippe Sutter

object Correlations {

  def main(args : Array[String]) {
    if(args.size > 0) {
      val in = parseFile(args(0))
      println("# Parsed " + in.size + " lines of dimension " + in(0).size + ".")
      val m = buildMatrix(in, impl, false)

      println("# Correlation matrix: ")

      for(line <- m) {
        println(line.map(v => "%1.2f".format(v)).mkString(" "))
      }
    } else {
      Console.println("No file given, I'll be using a random matrix for demo purposes.")
      val m = buildMatrix(Data.randomSet, corr, true)

      for(line <- m) {
        println(line.map(v => "%1.2f".format(v)).mkString(" "))
      }
    }
  }

  def parseFile(fileName : String) : Seq[Seq[Boolean]] = {
    val lines = scala.io.Source.fromFile(fileName).getLines

    var lineSz = -1

    (for(line <- lines) yield {
      val boolArray : Seq[Boolean] = line.split(" ").tail.map(_ match {
        case "1" => true
        case "0" => false
        case _ => sys.error("Expected 0 or 1 here.")
      }).toSeq

      if(lineSz >= 0) {
        assert(boolArray.size == lineSz)
      } else {
        lineSz = boolArray.size
      }

      boolArray
    }).toSeq
  }

  // Takes a list (lines) of lists (columns) of booleans values and computes a correlation matrix (or another measure) for the columns.
  def buildMatrix(values : Seq[Seq[Boolean]], measure : (Seq[Boolean],Seq[Boolean])=>Double, isSymmetric : Boolean = false) : Array[Array[Double]] = {
    assert(values.size > 0)
    val entries : Int = values.size
    val size    : Int = values(0).size

    val matrix = Array.fill(size)(Array.fill(size)(0.0d))
    
    // We assume there will be 1s on the diagonal.
    for(x <- 0 until size) {
      matrix(x)(x) = 1.0d
    }

    // We cache the columns, so we need to extract each of them only once.
    // Eventually, "columns" will be the transposed matrix of "values".
    val columns : Array[List[Boolean]] = Array.fill(size)(Nil)

    def getOrBuildColumn(i : Int) : List[Boolean] = columns(i) match {
      case c @ (_ :: _) => c
      case Nil => {
        val lst = values.map(line => line(i)).toList
        columns(i) = lst
        lst
      }
    }

    for(x <- 0 until size; y <- 0 until size if y != x && (!isSymmetric || y > x)) {
      val colX = getOrBuildColumn(x)
      val colY = getOrBuildColumn(y)
      val m = measure(colX, colY)

      matrix(x)(y) = m

      if(isSymmetric) {
        matrix(y)(x) = m
      }
    }

    matrix
  }

  // Correlation between two sets of measures. This measure is symmetric, ie. corr(x,y) == corr(y,x).
  def corr(x: Seq[Boolean], y : Seq[Boolean]) : Double =
    dist(x, y, (a: Int, b: Int, c: Int, d: Int) => (a + d).toDouble / (a + b + c + d).toDouble)

  // Jaccard coefficient for two sets of measures. This is also symmetric.
  def jaccardCoef(x : Seq[Boolean], y : Seq[Boolean]) : Double = 
    dist(x, y, (a: Int, b: Int, c: Int, d: Int) => a.toDouble / (a + b + c + d).toDouble)

  // Measure of implication. This is NOT symmetric !
  def impl(x : Seq[Boolean], y : Seq[Boolean]) : Double = 
    dist(x, y, (a: Int, b: Int, c: Int, d: Int) => (a + c).toDouble / (a + b + c + d).toDouble)

  // Distance between two boolean vectors using a given metric.
  def dist(x : Seq[Boolean], y : Seq[Boolean], metric : (Int,Int,Int,Int)=>Double) : Double = {
    var a, b, c, d : Int = 0

    for(vs <- x zip y) vs match {
      case (true,  true)  => a += 1
      case (true,  false) => b += 1
      case (false, true)  => c += 1
      case (false, false) => d += 1
    }

    metric(a,b,c,d)
  }
}

object Data {
  import scala.util.Random

  // Generates a list of 120 vectors of dimension 15, filled with random values.
  val randomSet : List[List[Boolean]] = List.fill(120)(List.fill(15)(Random.nextBoolean))
}
