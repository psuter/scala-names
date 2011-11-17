package ch.epfl.lara.scalanames.features
import edu.smu.tspell.wordnet._


trait IsVerb extends MethodFeature {
  import component._
  import component.global._

    private val wordNetPath : String = "C:\\Program Files\\WordNet\\2.1\\dict" 
    val name = "Method name is a verb"
    protected lazy val database = WordNetDatabase.getFileInstance

      //TODO externalize wordNet into AnalysisComp
      
  protected def feature(methodDef:MethodDef): Boolean = {
       
    /* Contains a verb
     * contains = true
     * jumping  = true
     * walk     = false
     * jump     = false
     **/
    //val base : Array[String] = database.getBaseFormCandidates(methodDef.name,SynsetType.VERB)    
    
    val verb : Array[Synset] = database.getSynsets(methodDef.name, SynsetType.VERB)
    
    //if(verb.length > 0) true else false
    verb.length > 0
  }

      
  def appliesTo(methodDef: MethodDef): Boolean = { 
        System.setProperty("wordnet.database.dir", wordNetPath)
        feature(methodDef)
  }

}
