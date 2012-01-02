package ch.epfl.lara.scalanames

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{Plugin,PluginComponent}

class ScalaNamesPlugin(val global : Global) extends Plugin {
  val name = "scala-names"
  val description = "Name analysis tools"
    
  val printOption = "pOutput" 
  val printFeatureID = "pFeatureId"
  val analysis = "pAnalysis"  

  val analysisComponent = new AnalysisComponent(this) {
    val global : ScalaNamesPlugin.this.global.type = ScalaNamesPlugin.this.global
  }

  val killComponent = new KillComponent(this) {
    val global : ScalaNamesPlugin.this.global.type = ScalaNamesPlugin.this.global
  }

  val components = List[PluginComponent](analysisComponent, killComponent)
  
  // This function adds information to "scalac -help" when the plugin is used.
  override val optionsHelp : Option[String] = Some(
    "  -P:"+name+":"+printOption+"             Print results in outputfile" + "\n" +
    "  -P:"+name+":"+printFeatureID+"             Print the feature before the results" + "\n" +
    "  -P:"+name+":"+analysis+"             Run analysis on the specified file compared to the scala lib" + "\n"
  )

  // This function is called by the scala compiler with all -P:myplugin:* options.
  // Note that it only receives the * part.
  override def processOptions(options: List[String], error: String => Unit) {
    for(option <- options) option match {
      case x if(printOption==x) => { analysisComponent.printy = true }
      case x if(printFeatureID==x) => { analysisComponent.featureID = true }
      case x if(analysis==x) => { analysisComponent.analysis = true }
      case _ => error(name+" doesn't support option : " + option)
    }
  }
}


