package ch.epfl.lara.scalanames.features

trait RightAssociative extends NameFinishWith {
  import component._
  import component.global._

  override val name = "This method is right associative"
    
  override val pattern = ":"
    

}