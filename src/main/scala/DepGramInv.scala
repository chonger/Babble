import multitool._
import scala.collection.mutable.{HashMap,HashSet}

class DepGramInv[Context <: Lookup,Outcome <: Lookup](val dg : DepGramBase[Context,Outcome], val maxD : Int) { 

  val allContexts = dg.getAllContexts()
  
  val inputs = new HashMap[Context,List[(Context,Outcome)]]()
  
  allContexts.foreach(c => {
    val os = dg.outcomesSmooth(c)
    os.foreach(o => {
      dg.implies(c,o).foreach(c2 => {
        val e = (c,o) :: inputs.getOrElse(c2,Nil)
        inputs += c2 -> e
      })
    })
  })
  
  def startContext(w : String) = {
    val poss = new HashMap[Context,Double]()
    dg.cprobs.filter(_._1.hasWord(w)).foreach(x => poss += x._1 -> x._2)
    dg.normalize(poss)
    assert(poss.size > 0)
    Babble.sample(poss.iterator)
  }

  def generateUp(w : String) : DepNode = {
    
    var cur = startContext(w)
    
    var cPath = List[(Context,Outcome)]()

    while(cur != dg.rootContext) {

      val poss = new HashMap[(Context,Outcome),Double]()
      inputs(cur).foreach(x => {
        val p = dg.cprobs(x._1) * dg.probs(x._1)(x._2)
        poss += x -> p
      })

      dg.normalize(poss)
      val co = Babble.sample(poss.iterator)
      cPath ::= co
      cur = co._1
    }
    
    //cPath.foreach(x => println(x._1 + " -------> " + x._2))

    dg.composeChain(cPath,dg.chooseSmooth)

  }


    
    

  
  


}
