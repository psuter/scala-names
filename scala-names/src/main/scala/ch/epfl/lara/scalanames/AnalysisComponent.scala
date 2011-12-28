package ch.epfl.lara.scalanames

import scala.tools.nsc.{Global,Phase}
import scala.tools.nsc.plugins.PluginComponent
import scala.collection.mutable.{Map=>MutableMap,Set=>MutableSet}
import ch.epfl.lara.scalanames.features._
import java.io.BufferedWriter
import java.io.FileWriter
import edu.smu.tspell.wordnet.WordNetDatabase

abstract class AnalysisComponent(pluginInstance : ScalaNamesPlugin) extends PluginComponent with Definitions {
  val global : Global
  import global._

  override val runsRightAfter : Option[String] = Some("refchecks")
  override val runsAfter : List[String]        = List("refchecks")

  val phaseName = pluginInstance.name
  var printy = false		//Print in the output file
  var featureID = false		//Print in the output file the features ID

  class AnalysisPhase(prev : Phase) extends StdPhase(prev) {
    private val nameCollectors : MutableMap[CompilationUnit,NameCollector] = MutableMap.empty
    
    val testOutput = ".\\testOutput.txt"
    val libOutput = ".\\libOutput.txt"
    lazy val out = new BufferedWriter(new FileWriter(libOutput, true))

    val wordNetPath : String = "C:\\Program Files\\WordNet\\2.1\\dict" 
    System.setProperty("wordnet.database.dir", wordNetPath)
    lazy val db : WordNetDatabase = WordNetDatabase.getFileInstance()

    def apply(unit : CompilationUnit) : Unit = {
      val nc = new NameCollector(unit)
      nameCollectors(unit) = nc
      nc.collect
      
      import global.definitions.{ getClass => gc }
      
      val featureList : List[MethodFeature { val component : AnalysisComponent.this.type }] = List(
          new ReturnsTraversable{ 	   val id =  1 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
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
          new IsVerb{ val database = db ;
          							   val id = 16 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new IsNoun{ val database = db ;
          							   val id = 17 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new IsCamelPhrase{ 		   val id = 18 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new ContainsAcronym{		   val id = 19 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new AbstractPhrase{ val database = db ;
          							   val id = 20 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new NameContains{ val pattern = "is" ;
           							   val id = 21 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new NameContains{ val pattern = "get" ;
           							   val id = 22 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new NameContains{ val pattern = "set" ;
           							   val id = 23 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new NameContains{ val pattern = "contains" ;
           							   val id = 24 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new IsValidJavaName{		   val id = 25 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new IsOperator{		 	   val id = 26 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new ContainsCompleteReturnTypeInName{
          							   val id = 27 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new ContainsPartialReturnTypeInName{
           							   val id = 28 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new RightAssociative{ 	   val id = 29 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new IsInnerMethod{		   val id = 30 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new ContainsInnerMethod{ 	   val id = 31 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new IsOverride{		 	   val id = 32 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new IsAbstract{		 	   val id = 33 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new IsPublic{			 	   val id = 34 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new IsStatic{			 	   val id = 35 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new NameFinishWith{ val pattern = "s"
            						   val id = 36 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },
          new NameFinishWith{ val pattern = "ss"
            						   val id = 36 ; val component : AnalysisComponent.this.type = AnalysisComponent.this }

         // new InferedType{		 	   val id = 34 ; val component : AnalysisComponent.this.type = AnalysisComponent.this }


          //new IsInfinitiveVerb{        val id = 18 ; val component : AnalysisComponent.this.type = AnalysisComponent.this },

      

      )
 
      if(featureID){
          out.write("{\n")
    	  for(f <- featureList){
    		  try{
    			out.write(f.id+";"+f.name+"\n")
    		  } catch {
    		  	case e => println("I/O error ")
    	  }}
          out.write("}\n")
          out.flush
      }
      
      // check all instenciated features for all MethodDef found     
      for(defn <- nc.collectedDefinitions) {
        defn match {
          case md : MethodDef => {
            if(!md.synthetic) {
              val str =featureList.map(f => if(f.appliesTo(md)) 1 else 0).mkString(" ") + " " + md.UniqueName + "\n" 
              //Print into file
              if(printy){
                try{
                  out.write(str)
                  out.flush 
                } catch {
              	  case e => print("I/O error "+e.toString()+" during: "+str); printy=false
              }}              
              print(str)          
          }}
          case _ =>
      }}
      
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
            d.name.toString().equals("<init>") ||
            d.name.debugString().equals("$init$")||
            d.name.toString == "readResolve" //this is not synthetical, but it's not a method definition, so don't care
          )
          
          Some(MethodDef(name.debugString(),d,isSynth,d.pos))
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
          // ln("Mods : " + tree.asInstanceOf[MemberDef].mods)
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

