package ch.epfl.lara.scalanames

import scala.tools.nsc.{Global,Phase}
import scala.tools.nsc.plugins.PluginComponent
import scala.collection.mutable.{Map=>MutableMap,Set=>MutableSet}

abstract class AnalysisComponent(pluginInstance : ScalaNamesPlugin) extends PluginComponent {
  val global : Global
  import global._

  override val runsRightAfter : Option[String] = Some("refchecks")
  override val runsAfter : List[String]        = List("refchecks")

  val phaseName = pluginInstance.name

  class AnalysisPhase(prev : Phase) extends StdPhase(prev) {
    private val nameCollectors : MutableMap[CompilationUnit,NameCollector] = MutableMap.empty

    def apply(unit : CompilationUnit) : Unit = {
      val nc = new NameCollector(unit)
      nameCollectors(unit) = nc
      nc.collect
    }
  }

  def newPhase(prev : Phase) = new AnalysisPhase(prev)

object Definition {
    
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
  
  abstract class D(name : String, kind : DefKinds.DefKind, synthetic : Boolean, position : Position) {
    override def toString : String =
      (if(synthetic) "(S) " else "    ") + kind + " " + name + " @" + position
    def getS: Boolean = synthetic
    def getK: DefKinds.DefKind = kind
    
  }
  
  //Temporary case class
  case class Any(name : String, kind : DefKinds.DefKind, synthetic : Boolean, position : Position) extends D(name,kind,synthetic,position){
  }
  
  case class Parameter(name: String, ptype: Type, synthetic : Boolean, position : Position) extends D(name,DefKinds.Param,synthetic,position){
    override def toString : String = super.getK +" "+name +":"+ptype

  }
  
  case class MethodDef(name: String, args: List[Parameter], rettype: Type, synthetic: Boolean, position: Position) 
  extends D(name,DefKinds.Def,synthetic,position) {
    override def toString : String = 
      (if(synthetic) "(S) " else "    ") + DefKinds.Def.toString + " " + name + 
      (if(args.isEmpty)"()" else "("+prettyArgs(args)+")")+":"+ rettype+" @" + position
      
      //Print prettier the args
      def prettyArgs(list:List[Parameter]):String= list match{
      case x :: xs => x.name+":"+x.ptype+","+prettyArgs(xs)
      case Nil => ""
      }
  }


}

  class NameCollector(val unit : CompilationUnit) extends Traverser {
    import Definition.DefKinds._
    import Definition._
    private var collected = false
    def collect() {
      if(!collected) {
        collected = true
        traverse(unit.body)
      }
    }
    
    def mapParam(ss: List[Symbol],ts: List[Type], res:List[Parameter]): List[Parameter] = ss match {
      case x :: xs => mapParam(xs,ts.tail,Parameter(x.name.toString,ts.head,x.isSynthetic,x.pos)::res)
      case Nil => res
  }

    private val definitions : MutableSet[D] = MutableSet.empty
  
    override def traverse(tree : Tree) {
      val optDfn = tree match {
        case d @ TypeDef(mods, _, _, _) => {
          Some(Any(d.name.toString, Type, mods.isSynthetic, d.pos))
        }
        case d @ ClassDef(mods, _, _, _) => {
          if(mods.hasModuleFlag) {
            Some(Any(d.name.toString, Object, mods.isSynthetic, d.pos))
          } else {
            Some(Any(d.name.toString, if(mods.isTrait) Trait else Class, mods.isSynthetic, d.pos))
          }
        }
        /**DefDef (mods: Modifiers, name: TermName, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree)
         * extends ValOrDefDef with Product with Serializable **/
        case d @ DefDef(mods, name, _, _, _, _) => {
          val isSynth = (
            mods.isSynthetic ||
            mods.hasAccessorFlag ||
            mods.isParamAccessor ||
            mods.isCaseAccessor ||
            mods.isSuperAccessor
          )         
          Some(MethodDef(name.toString,mapParam(d.symbol.tpe.params,d.symbol.tpe.paramTypes,List()),d.symbol.tpe.resultType,isSynth,d.pos))
        }
        case d @ ModuleDef(mods, _, _) => {
          Some(Any(d.name.toString, Object, mods.isSynthetic, d.pos))
        }
        case d @ PackageDef(_, _) => {
          Some(Any(d.name.toString, Package, false, d.pos))
        }
        case d @ ValDef(mods, _, _, _) => {
          if(mods.isParameter) {
            Some(Parameter(d.name.toString, d.symbol.tpe.resultType, mods.isSynthetic, d.pos))
          } else {
            Some(Any(d.name.toString, if(mods.isMutable) Var else Val, mods.isSynthetic, d.pos))
          }
        }
        case _ => None
      }

      optDfn match {
        case Some(dfn) => {
          println(dfn)
          // println("Mods : " + tree.asInstanceOf[MemberDef].mods)
          definitions += dfn

          // To avoid collecting arguments of synthetic methods, for instance.
          if(!dfn.getS) {
            super.traverse(tree)
          }
        }
        case None => super.traverse(tree)
      }
    }
  }
}
