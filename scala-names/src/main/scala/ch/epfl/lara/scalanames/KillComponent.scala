package ch.epfl.lara.scalanames

import scala.tools.nsc.{Global,Phase}
import scala.tools.nsc.plugins.PluginComponent

abstract class KillComponent(pluginInstance : ScalaNamesPlugin) extends PluginComponent {
  val global : Global
  import global._

  override val runsRightAfter : Option[String] = Some("scala-names")
  override val runsAfter : List[String]        = List("scala-names")

  val phaseName = "STOP!"

  class KillPhase(prev : Phase) extends StdPhase(prev) {
    def apply(unit : CompilationUnit) : Unit = {
      println("Killing the compiler now.")
      sys.exit(0)
    }
  }

  def newPhase(prev : Phase) = new KillPhase(prev)
}
