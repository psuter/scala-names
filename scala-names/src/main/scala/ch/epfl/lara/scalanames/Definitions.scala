package ch.epfl.lara.scalanames

import scala.tools.nsc.Global

trait Definitions {
  self : AnalysisComponent =>
  import global._
    
  object DefKinds extends Enumeration {
    type DefKind = Value

    val Type    = Value("type")
    val Class   = Value("class")
    val Trait   = Value("trait")
    val Def     = Value("def")
    val Object  = Value("object")
    val Package = Value("package")
    val Val     = Value("val")
    val Var     = Value("var")
    val Param   = Value("par")
  }
  
  abstract class D(val name : String, val kind : DefKinds.DefKind, val synthetic : Boolean, val position : Position) {
    override def toString : String =
      (if(synthetic) "(S) " else "    ") + kind + " " + name + " @" + position
  }
  
  //Temporary case class
  case class Any(
      override val name : String,
      override val kind : DefKinds.DefKind,
      override val synthetic : Boolean,
      override val position : Position) extends D(name,kind,synthetic,position){}
  
  case class Parameter(
      override val name: String,
      ptype: Type,
      override val synthetic : Boolean,
      override val position : Position) extends D(name,DefKinds.Param,synthetic,position){
      override def toString : String = "    " + kind + " "+name +":"+ptype+" @"+position

  }

  /**DefDef (mods: Modifiers, name: TermName, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree)
         * extends ValOrDefDef with Product with Serializable 
         * e.g. foo[A,B](x : Int)(y : A) : String = expr 
         *     name = "foo"
         *     tparams = List("A","B")
         *     vparamss = List(List("x : Int"), List("y : A")) //size 1: empty, size 0: , size > 1
         *     tpt = "String"
         *     rhs = [expr...] **/
    case class MethodDef(
      override val name: String,
      val d: DefDef,
      override val synthetic: Boolean,
      override val position: Position) extends D(name,DefKinds.Def,synthetic,position) { 
      
    	override def toString : String = 
    		(if(synthetic) "(S) " else "    ")+DefKinds.Def.toString+" "+name+"("+prettyArgs(args)+"):"+rettype+" @"+position
    		
    	def UniqueName: String = name+"@"+position
  
    	def rettype: Type = d.symbol.tpe.resultType
    	
    	lazy val args : List[Parameter] = computeArgs      
    	private def computeArgs: List[Parameter] = mapParam(d.symbol.tpe.params,d.symbol.tpe.paramTypes,List()) //TODO test behavior needed
        //Used to map symbol and type of parameters of this method to a list of Parameter
    	private def mapParam(ss: List[Symbol],ts: List[Type], res:List[Parameter]): List[Parameter] = ss match {
      		case x :: xs => mapParam(xs,ts.tail,Parameter(x.name.toString,ts.head,x.isSynthetic,x.pos)::res)
      		case Nil => res
    	}
      	//Print prettier the args
      	private def prettyArgs(list:List[Parameter]):String= list match{
      		case x :: Nil => x.name+":"+x.ptype
      		case x :: xs => x.name+":"+x.ptype+","+prettyArgs(xs)
      		case Nil => ""
      	}
    
    }
}