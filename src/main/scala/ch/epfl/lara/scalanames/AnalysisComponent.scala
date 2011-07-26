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
  case class D(name : String, kind : DefKinds.DefKind, synthetic : Boolean) {
    override def toString : String = {
      (if(synthetic) "(S) " else "    ") + kind + " " + name 
    }
  }

  class NameCollector(val unit : CompilationUnit) extends Traverser {
    import DefKinds._
    private var collected = false
    def collect() {
      if(!collected) {
        collected = true
        traverse(unit.body)
      }
    }

    private val definitions : MutableSet[D] = MutableSet.empty
  
    override def traverse(tree : Tree) {
      val optDfn = tree match {
        case d @ TypeDef(mods, _, _, _) => {
          Some(D(d.name.toString, Type, mods.isSynthetic))
        }
        case d @ ClassDef(mods, _, _, _) => {
          if(mods.hasModuleFlag) {
            Some(D(d.name.toString, Object, mods.isSynthetic))
          } else {
            Some(D(d.name.toString, if(mods.isTrait) Trait else Class, mods.isSynthetic))
          }
        }
        case d @ DefDef(mods, _, _, _, _, _) => {
          val isSynth = (
            mods.isSynthetic ||
            mods.hasAccessorFlag ||
            mods.isParamAccessor ||
            mods.isCaseAccessor ||
            mods.isSuperAccessor
          )
          Some(D(d.name.toString, Def, isSynth))
        }
        case d @ ModuleDef(mods, _, _) => {
          Some(D(d.name.toString, Object, mods.isSynthetic))
        }
        case d @ PackageDef(_, _) => {
          Some(D(d.name.toString, Package, false))
        }
        case d @ ValDef(mods, _, _, _) => {
          if(mods.isParameter) {
            Some(D(d.name.toString, Param, mods.isSynthetic))
          } else {
            Some(D(d.name.toString, if(mods.isMutable) Var else Val, mods.isSynthetic))
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
          if(!dfn.synthetic) {
            super.traverse(tree)
          }
        }
        case None => super.traverse(tree)
      }
    }
  }
}
