package ch.epfl.lara.scalanames.features

trait ContainsIf extends MethodFeature {
  import component._
  import component.global._

  val name = "Method contains IF statement."
  
  def appliesTo(methodDef: MethodDef): Boolean = { 
    
    def apply(t: Tree): Boolean = t match {
      case Block(stats,_) => applyAll(stats) 
      case CaseDef(pat,guard,body) => apply(pat) || apply(guard) || apply(body)
      case DefDef(_,_,_,_,_,rhs) => apply(rhs) //do we need to also catch If into parameters ?
      case Function(_,body) => apply(body)
      case i:If => true
      //case LabelDef
      case Match(selector,cases) => apply(selector) || applyAll(cases)
      //case ModuleDef
      //case PackageDef
      case Return(expr) => apply(expr)
      case Throw(expr) => apply(expr)
      case Try(block,catches,finalizer) => apply(block) || applyAll(catches) || apply(finalizer)
      case TypeDef(_,_,_,rhs) => apply(rhs) //do we need to also catch If into parameters ?
      case ValDef(_,_,_,rhs) => apply(rhs)
      case _ => false
    }
    
    def applyAll(ts: List[Tree]): Boolean = ts match {
      case x :: xs => apply(x) || applyAll(xs)
      case Nil => false
    }
    
    apply(methodDef.d.rhs)
  }
}