package ch.epfl.lara.scalanames

import scala.tools.nsc.Global

trait Definitions extends ReturnsTraversable {
  val global : Global
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
      override val position : Position) extends D(name,kind,synthetic,position){
    
  }
  
  case class Parameter(
      override val name: String,
      ptype: Type,
      override val synthetic : Boolean,
      override val position : Position) extends D(name,DefKinds.Param,synthetic,position){
    override def toString : String = "    " + kind + " "+name +":"+ptype+" @"+position

  }
  
  case class MethodDef(
      override val name: String,
      args: List[Parameter],
      rettype: Type,
      override val synthetic: Boolean,
      override val position: Position) extends D(name,DefKinds.Def,synthetic,position) {
    override def toString : String = 
      (if(synthetic) "(S) " else "    ")+DefKinds.Def.toString+" "+name+"("+prettyArgs(args)+"):"+rettype+" @"+position
  
      
      //Print prettier the args
    def prettyArgs(list:List[Parameter]):String= list match{
      case x :: Nil => x.name+":"+x.ptype
      case x :: xs => x.name+":"+x.ptype+","+prettyArgs(xs)
      case Nil => ""
      }
    
  }


}