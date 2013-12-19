class NGramContext(val prev : List[(Int,Int)]) extends Lookup {
  override def equals(any : Any) : Boolean = {
	any match {
	  case t : NGramContext => prev == t.prev
	  case _ => false
	}	
  }
  override def hashCode() : Int = prev.hashCode()
  override def inLexicon(lexicon : Lexicon) : Boolean = {
    (true /: prev)((a,b) => {
      a && (b._1 == -1 || b._2 == -1 || DepNode.words(b._2) == "ROOT" || lexicon.inVocab(DepNode.words(b._2).toLowerCase()))
    })
  }

  override def toString() : String = {
    prev.map(x => {
      val t = DepNode.postags(x._1) 
      val w = if(x._2 == -1) "<>" else DepNode.words(x._2) 
      t + "/" + w
    }).toArray.mkString(" ")
  }

  override def hasWord(w : String) : Boolean = {
//    println(w.toLowerCase() + "/" + DepNode.words(cur._2).toLowerCase())
    DepNode.words(prev(0)._2).toLowerCase() == w.toLowerCase()
  }


}

class NGramOutcome(val pos : Int, val w : Int) extends Lookup {
  override def equals(any : Any) : Boolean = {
	any match {
	  case t : NGramOutcome => pos == t.pos && w == t.w
	  case _ => false
	}	
  }
  override def hashCode() : Int = pos.hashCode() ^ w.hashCode()
  override def inLexicon(lexicon : Lexicon) : Boolean = {
    pos == -1 || lexicon.inVocab(DepNode.words(w).toLowerCase())
  }
}


class NGram(n : Int) extends DepGramBase[NGramContext,NGramOutcome] {

  import scala.collection.mutable.HashMap

  override def getObservations(t : DepNode) = {

    val seq = t.seq()

    var last = rootContext
    var ret = seq.map(x => {

      val context = last

      val outcome = new NGramOutcome(x._1,x._2)
      
      last = new NGramContext((x :: context.prev).take(n-1))

      (context,outcome)
    })

    (last,new NGramOutcome(-1,-1)) :: ret
  }

  override def implies(c : NGramContext, o : NGramOutcome) : List[NGramContext] = {
    if(o.pos == -1) {
      List[NGramContext]()
    } else {
      List[NGramContext](new NGramContext(((o.pos,o.w) :: c.prev).take(n-1)))
    }
  }

  override def rootContext = {
    new NGramContext(List((DepNode.postags.add("ROOT"),DepNode.words.add("ROOT"))))
  }
    
  override def compose(k : NGramContext, f : (NGramContext) => NGramOutcome) : DepNode = {    
    val outcome = f(k)
    var cur = k.prev(0)
    if(outcome.pos != -1) {
      val kk = compose(new NGramContext(((outcome.pos,outcome.w) :: k.prev).take(n-1)),f)
      new DepNode(cur._1,cur._2,Nil,List(kk))
    } else {
      new DepNode(cur._1,cur._2,Nil,Nil)
    }
  }

  override def smoothC(c : NGramContext) : Any = c.prev.map(_._1)
  override def smoothO(c : NGramOutcome) : Any = c.pos

}


