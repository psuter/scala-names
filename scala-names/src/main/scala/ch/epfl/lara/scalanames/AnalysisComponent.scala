package ch.epfl.lara.scalanames

import scala.tools.nsc.{Global,Phase}
import scala.tools.nsc.plugins.PluginComponent
import scala.collection.mutable.{Map=>MutableMap,Set=>MutableSet}
import ch.epfl.lara.scalanames.features._

abstract class AnalysisComponent(pluginInstance : ScalaNamesPlugin) extends PluginComponent with Definitions {
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
      
      val featureList : List[MethodFeature { val component : AnalysisComponent.this.type }] = List(
          new ReturnSubtypeOf { val traitSymbol = global.definitions.getClass("scala.collection.Traversable");
          						val id = 1 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new ReturnSubtypeOf { val traitSymbol = global.definitions.getClass("scala.AnyRef");
          						val id = 2 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new ReturnTypeIs { val ttype = SupportedType.Unit;
          						val id = 3 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new ReturnTypeIs { val ttype = SupportedType.Boolean;
          						val id = 4 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new ReturnTypeIs { val ttype = SupportedType.Int;
          						val id = 5 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new ReturnTypeIs { val ttype = SupportedType.String;
          						val id = 6 ; val component : AnalysisComponent.this.type = AnalysisComponent.this }
          
      )
      
      // check all instenciated features for all MethodDef
      for(defn <- nc.collectedDefinitions) {
        defn match {
          case md : MethodDef => {
            println(md.name + " " + featureList.map(f => if(f.appliesTo(md)) 1 else 0).mkString(" "))
          }
          case _ => ;
        }
        
      }
    }
  }

  def newPhase(prev : Phase) = new AnalysisPhase(prev)

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
    def collectedDefinitions : Set[D] = Set(definitions.toSeq : _*)
  
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
        case d @ DefDef(mods, name, _, _, _, _) => {
          val isSynth = (
            mods.isSynthetic ||
            mods.hasAccessorFlag ||
            mods.isParamAccessor ||
            mods.isCaseAccessor ||
            mods.isSuperAccessor ||           
            d.name.toString().equals("<init>")
          )         
          //Some(MethodDef(name.toString,mapParam(d.symbol.tpe.params,d.symbol.tpe.paramTypes,List()),d.symbol.tpe.resultType,isSynth,d.pos))
          Some(MethodDef(name.toString,d,isSynth,d.pos))
        }
        case d @ ModuleDef(mods, _, _) => {
          Some(Any(d.name.toString, Object, mods.isSynthetic, d.pos))
        }
        case d @ PackageDef(_, _) => {
          //val isSynth = d.name.toString().equals("<empty>")    But if do so, compiler stop fetching data   
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
          if(!dfn.synthetic) {
            super.traverse(tree)
          }
        }
        case None => super.traverse(tree)
      }
      
    }
  }
}

