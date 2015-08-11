object NGramContext {

  def load(s : String) = {
    val ns = s.split("\\s").sliding(2,2)
    new NGramContext(ns.map(n => {
      val pos = DepNode.postags.add(n(0))
      val w = if(n(1) == "-1") -1 else DepNode.words.add(n(1))
      (pos,w)
    }).toList)
  }

}

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
    DepNode.words(prev(0)._2).toLowerCase() == w.toLowerCase()
  }

  override def save() : String = {
    prev.map({
      case (pos,w) => {
        val pS = if(pos == -1) "-1" else DepNode.postags(pos) 
        val wS = if(w == -1) "-1" else DepNode.words(w) 
        pS + "\t" + wS
      }
    }).toArray.mkString("\t")
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
  override def save() : String = {
    DepNode.postags(pos) + "\t" + DepNode.words(w)
  }
}


class NGram(n : Int) extends Babbler[NGramContext,NGramOutcome] {

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
    
  override def realize(n : CONode) = {
    var w = DepNode.words(n.c.prev(0)._2)
    if(w == "ROOT") 
      realize(n.i(0))
    else
      w + " " + realize(n.i(0))
  }

  override def smoothC(c : NGramContext) : Any = c.prev.map(_._1)
  override def smoothO(c : NGramOutcome) : Any = c.pos
  override def smoothImplies(a : Any, o : NGramOutcome) : List[Any] = {
    List((o.pos :: a.asInstanceOf[List[(Int,Int)]]).take(n-1))
  }
}


class NGram53 extends NGram(3) {
  override def smoothC(c : NGramContext) : Any = c//.prev.take(1)
  override def smoothO(c : NGramOutcome) : Any = c
  override def smoothImplies(a : Any, o : NGramOutcome) : List[Any] = {
    implies(a.asInstanceOf[NGramContext],o)
  }
}

