package ch.epfl.lara.scalanames

import scala.tools.nsc.{Global,Phase}
import scala.tools.nsc.plugins.PluginComponent
import scala.collection.mutable.{Map=>MutableMap,Set=>MutableSet}
import ch.epfl.lara.scalanames.features._
import java.io.BufferedWriter
import java.io.FileWriter

abstract class AnalysisComponent(pluginInstance : ScalaNamesPlugin) extends PluginComponent with Definitions {
  val global : Global
  import global._

  override val runsRightAfter : Option[String] = Some("refchecks")
  override val runsAfter : List[String]        = List("refchecks")

  val phaseName = pluginInstance.name

  class AnalysisPhase(prev : Phase) extends StdPhase(prev) {
    private val nameCollectors : MutableMap[CompilationUnit,NameCollector] = MutableMap.empty
    val output: String = ".\\output.txt"
    lazy val out = new BufferedWriter(new FileWriter(output, true))

    def apply(unit : CompilationUnit) : Unit = {
      val nc = new NameCollector(unit)
      nameCollectors(unit) = nc
      nc.collect
      
      import global.definitions.{ getClass => gc }
      
      val featureList : List[MethodFeature { val component : AnalysisComponent.this.type }] = List(
          new ReturnSubtypeOf { val traitSymbol = gc("scala.collection.Traversable");
          							   val id =  1 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new ReturnSubtypeOf { val traitSymbol = gc("scala.AnyRef");
          							   val id =  2 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new ReturnTypeIs { val traitSymbol = gc("scala.Unit");
          							   val id =  3 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new ReturnTypeIs { val traitSymbol = gc("scala.Boolean");
          							   val id =  4 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new ReturnTypeIs { val traitSymbol = gc("scala.Int");
          							   val id =  5 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new ReturnTypeIs { val traitSymbol = gc("java.lang.String");
          							   val id =  6 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new NoParam { 			   val id =  7 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new HasNoParentesis {        val id =  8 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new ContainsIf { 			   val id =  9 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new ContainsWhile {		   val id = 10 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new ContainsTryCatch {	   val id = 11 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new ContainsPatternMatching {val id = 12 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new ThrowException {		   val id = 13 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new IsCurrified{			   val id = 14 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new ContainsSelfRecursion{   val id = 15 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new IsVerb{   			   val id = 16 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new IsNoun{ 				   val id = 17 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          //new IsInfinitiveVerb{        val id = 18 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new IsCamelPhrase{ 		   val id = 19 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new ContainsAcronym{		   val id = 20 ; val component : AnalysisComponent.this.type = AnalysisComponent.this }

      )
      
      // check all instenciated features for all MethodDef found
      println
      
      for(defn <- nc.collectedDefinitions) {
        defn match {
          case md : MethodDef => {
            if(!md.synthetic) {
              try{
                val str = md.name + " " + featureList.map(f => if(f.appliesTo(md)) 1 else 0).mkString(" ") + "\n"
                
               //Print into file
               /* out.write(str)
                out.flush 
               */
                print(str)
              } catch {
              	case e => println("I/O error "+e.toString()+" during: "+md.name + " " + featureList.map(f => if(f.appliesTo(md)) 1 else 0).mkString(" "))
              }
            }
          }
          case _ =>
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

