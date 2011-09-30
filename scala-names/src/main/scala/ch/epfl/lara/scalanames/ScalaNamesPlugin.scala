package ch.epfl.lara.scalanames

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{Plugin,PluginComponent}

class ScalaNamesPlugin(val global : Global) extends Plugin {
  val name = "scala-names"
  val description = "Name analysis tools"

  val analysisComponent = new AnalysisComponent(this) {
    val global : ScalaNamesPlugin.this.global.type = ScalaNamesPlugin.this.global
  }

  val killComponent = new KillComponent(this) {
    val global : ScalaNamesPlugin.this.global.type = ScalaNamesPlugin.this.global
  }

  val components = List[PluginComponent](analysisComponent, killComponent)
}


