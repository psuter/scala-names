package ch.epfl.lara.scalanames.features
import edu.smu.tspell.wordnet._

trait AbstractPhrase extends ContainsAcronym {
  import component._
  import component.global._
  
  override val name = "Method name match abstract phrase construction."
    
  val database : WordNetDatabase
    
  override def appliesTo(methodDef: MethodDef): Boolean = {

    val cp = reconstructAcronym(methodDef.name)    
    if(cp.length > 0) validAbstractPhrase(cp) else false
  }
  
  /** Considered as valid camel phrase:
   *   get -> noun
   * | set -> noun
   * | contains -> noun
   * | is -> adjective
   * | _ -> _
   */
  def validAbstractPhrase(cp: List[String]): Boolean = cp match {
    case Nil => true
    case x :: Nil => true
    case x1 :: (x2 :: xs) => x1.toLowerCase() match {
      case "get" | "set" | "contains" => {
        val noun = database.getSynsets(x2, SynsetType.NOUN)
        if(noun.length > 0 || isAcronym(x2)) validAbstractPhrase(xs) else false
      }
      case "is" => {
        val adjectiv = database.getSynsets(x2, SynsetType.ADJECTIVE)
        if (adjectiv.length > 0 || isAcronym(x2)) validAbstractPhrase(xs) else false
      }
      case _ => validAbstractPhrase(x2::xs)
    }
  }
	 
  
}