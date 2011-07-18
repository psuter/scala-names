package ch.epfl.lara.scalanames

import scala.tools.nsc.{Global,Phase}
import scala.tools.nsc.plugins.PluginComponent

abstract class AnalysisComponent(pluginInstance : ScalaNamesPlugin) extends PluginComponent {
  val global : Global
  import global._

  override val runsRightAfter : Option[String] = Some("refchecks")
  override val runsAfter : List[String]        = List("refchecks")

  val phaseName = pluginInstance.name

  class AnalysisPhase(prev : Phase) extends StdPhase(prev) {
    def apply(unit : CompilationUnit) : Unit = {
      println("Phase ran.")
    }
  }

  def newPhase(prev : Phase) = new AnalysisPhase(prev)
}
