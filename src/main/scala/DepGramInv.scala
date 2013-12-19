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
    val poss = allContexts.filter(_.hasWord(w)).toSeq
    assert(poss.length > 0)
    //println("POSSIBLE STARTS = " + poss.length)
    poss(Babble.rando.nextInt(poss.length))
  }

  def generateUp(w : String) : DepNode = {
    
    var cur = startContext(w)
    
    var cPath = List[(Context,Outcome)]()

    while(cur != dg.rootContext) {
      val co = inputs(cur)(Babble.rando.nextInt(inputs(cur).length))
      cPath ::= co
      cur = co._1
    }
    
    //cPath.foreach(x => println(x._1 + " -------> " + x._2))


    dg.composeChain(cPath,dg.chooseSmooth)

  }


    
    

  
  


}
