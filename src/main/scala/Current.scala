import multitool._
import scala.collection.mutable.{HashMap,HashSet}


class PLContext(val pTags : List[Int], val cur : (Int,Int)) extends Lookup {
  override def equals(any : Any) : Boolean = {
	any match {
	  case t : PLContext => pTags == t.pTags && cur == t.cur
	  case _ => false
	}	
  }
  override def hashCode() : Int = cur.hashCode() ^ pTags.hashCode()

  override def toString() : String = {
    pTags.toArray.map(x => DepNode.postags(x)).mkString(" ") + " " + DepNode.postags(cur._1) + " / " + DepNode.words(cur._2)
  }

  override def inLexicon(lexicon : Lexicon) : Boolean = {
    val word = DepNode.words(cur._2)
    word == "ROOT" || lexicon.inVocab(word.toLowerCase())
  }


}


class AllOutcome(val left : List[(Int,Int)], val right : List[(Int,Int)]) extends Lookup {
  override def equals(any : Any) : Boolean = {
	any match {
	  case t : AllOutcome => left == t.left && right == t.right
	  case _ => false
	}	
  }
  override def hashCode() : Int = left.hashCode() ^ right.hashCode()
  override def toString() : String = {
    var r = left.map(x => DepNode.postags(x._1) + "/" + DepNode.words(x._2)).toArray.mkString(" ")
    r += " || "
    r += right.map(x => DepNode.postags(x._1) + "/" + DepNode.words(x._2)).toArray.mkString(" ")
    r
  }

  override def inLexicon(lexicon : Lexicon) : Boolean = {
    (left ::: right).foreach(w => {
      if(!lexicon.inVocab(DepNode.words(w._2).toLowerCase())) {
        return false
      }
    })
    true
  }
}

class AllDepsL extends DepGramBase[PLContext,AllOutcome] {

  import scala.collection.mutable.HashMap

  override def rootContext() = {
    new PLContext(List[Int](),(DepNode.postags.add("ROOT"),DepNode.words.add("ROOT")))
  }

  override def getObservations(t : DepNode) = {

    val pMap = new HashMap[DepNode,DepNode]()
      
    t.under().foreach(n => {
      (n.left ::: n.right).foreach(nn => {
        pMap += nn -> n
      })
    })
    
    t.under().map(n => {
      var parents = List[Int]()
      var cur = n
      while(cur != null) {
        val p = pMap.getOrElse(cur,null)
        if(p != null)
          parents ::= p.pos
        cur = p
      }
      
      val context = new PLContext(parents.reverse,(n.pos,n.w))
      val outcome = new AllOutcome(n.left.map(x => (x.pos,x.w)),n.right.map(x => (x.pos,x.w)))
      (context,outcome)
    })

  }

  override def implies(c : PLContext, o : AllOutcome) : List[PLContext] = {
    (o.left ::: o.right).map(x => {
      new PLContext(c.cur._1 :: c.pTags,x)
    })
  }
  
  override def compose(k : PLContext, f : (PLContext) => AllOutcome) : DepNode = {
    val outcome = f(k)
    new DepNode(k.cur._1,k.cur._2,
                outcome.left.map(x => compose(new PLContext(k.cur._1 :: k.pTags,x),f)),
                outcome.right.map(x => compose(new PLContext(k.cur._1 :: k.pTags,x),f)))
  }

  override def smoothC(c : PLContext) : Any = c.cur._1 :: c.pTags
  override def smoothO(o : AllOutcome) : Any = o.left.map(_._1) ::: List(-1) ::: o.right.map(_._1)

}


class AllWords(lexicon : Lexicon) extends DepGramBase[NGramContext,AllOutcome] {

  import scala.collection.mutable.HashMap

  override def rootContext = {
    new NGramContext(List((DepNode.postags.add("ROOT"),DepNode.words.add("ROOT"))))
  }
  
  override def getObservations(t : DepNode) = {

    val pMap = new HashMap[DepNode,DepNode]()
      
    t.under().foreach(n => {
      (n.left ::: n.right).foreach(nn => {
        pMap += nn -> n
      })
    })
    
    t.under().map(n => {
      var parents = List[(Int,Int)]()
      var cur = n
      while(cur != null) {
        val p = pMap.getOrElse(cur,null)
        if(p != null)
          parents ::= (p.pos,p.w)
        cur = p
      }
      
      var plist = (n.pos,n.w) :: parents.reverse
      val context = new NGramContext(filt(plist))
      val outcome = new AllOutcome(n.left.map(x => (x.pos,x.w)),n.right.map(x => (x.pos,x.w)))
      (context,outcome)
    })

  }

  def filt(plist : List[(Int,Int)]) : List[(Int,Int)] = plist

  override def implies(c : NGramContext, o : AllOutcome) : List[NGramContext] = {
    (o.left ::: o.right).map(x => {
      new NGramContext(filt(x :: c.prev))
    })
  }
  
  override def compose(k : NGramContext, f : (NGramContext) => AllOutcome) : DepNode = {
    val outcome = f(k)
    val cur = k.prev(0)
    new DepNode(cur._1,cur._2,
                outcome.left.map(x => compose(new NGramContext(filt(x :: k.prev)),f)),
                outcome.right.map(x => compose(new NGramContext(filt(x :: k.prev)),f)))
  }  

  //override def smoothC(c : NGramContext) : Any = c.prev.map(_._1)
  //override def smoothO(o : AllOutcome) : Any = o.left.map(_._1) ::: List(-1) ::: o.right.map(_._1)

  override def smoothC(c : NGramContext) : Any = c.prev.map({
    case (npos,nw) => {
      wNetSmooth(npos,nw)
    }
  })

  def wNetSmooth(npos : Int, nw : Int) : (Int,Int) = {
    if(nw == -1) {
      (npos,-1)
    } else {
      val w = DepNode.words(nw)
      val pos = DepNode.postags(npos)
      val h = lexicon.addHNym(w,pos)
      if(h == -1)
        (npos,nw)
      else
        (npos,h)
    }
  }

  override def smoothO(o : AllOutcome) : Any = o.left.map(x => wNetSmooth(x._1,x._2)) ::: List((-5,-5)) ::: o.right.map(x => wNetSmooth(x._1,x._2))

  override def composeChain(chain : List[(NGramContext,AllOutcome)], f : (NGramContext) => AllOutcome) : DepNode = {
    val (c,outcome) = chain(0)
    var nextChain = chain.drop(1)
    var cur = c.prev(0)

    //println(c + " ---> " + outcome)

    val leftCs = outcome.left.map(x => new NGramContext(filt(x :: c.prev)))
    val rightCs = outcome.right.map(x => new NGramContext(filt(x :: c.prev)))
    /**
    println("L")
    leftCs.map(x => println(x))
    println("R")
    rightCs.map(x => println(x))
*/
    if(nextChain.length > 0) {
      var targ = nextChain(0)._1

      //println("TARG " + targ)
      val poss = (leftCs ::: rightCs).filter(_ == targ)
      val ch = poss(Babble.rando.nextInt(poss.length))
      new DepNode(cur._1,cur._2,
                  leftCs.map(x => if(x eq ch) composeChain(nextChain,f) else compose(x,f)),
                  rightCs.map(x => if(x eq ch) composeChain(nextChain,f) else compose(x,f)))
    } else {
      new DepNode(cur._1,cur._2,
                  leftCs.map(x => compose(x,f)),
                  rightCs.map(x => compose(x,f)))
    }
    
  }

  override def smoothImplies(a : Any, o : AllOutcome) : List[Any] = {
    a match {
      case cs : List[(Int,Int)] => {
        (o.left ::: o.right).map(x => {
          filt(wNetSmooth(x._1,x._2) :: cs)
        })
      }
    }
  }

}

class AllWordsF(lexicon : Lexicon) extends AllWords(lexicon) {

  override def smoothImplies(a : Any, o : AllOutcome) : List[Any] = {
    a match {
      case cs : List[(Int,Int)] => {
        (o.left ::: o.right).map(x => {
          filt(wNetSmooth(x._1,x._2) :: cs)
        })
      }
    }
  }

  /**
   *
   *   Cuts upwards context chain at adj's, Nouns, Verbs, Prepositions, and root
   *   includes parent nonterminal of cutpoint
   * 
   */ 

  override def filt(pList : List[(Int,Int)]) : List[(Int,Int)] = {
    var res = List[(Int,Int)]()
    var ind = 0
    pList.foreach(x => {
      res ::= x
      val tag = DepNode.postags(x._1)
      val l1 = tag.charAt(0)
      if(tag == "PRP" || l1 == 'N' || l1 == 'V' || tag == "ROOT" || l1 == 'J') {

        if(ind < pList.length-1) { 
          res ::= (pList(ind+1)._1,-1)
        }

/**
        println(new NGramContext(pList))
        println("----->")
        println(new NGramContext(res.reverse))
        readLine()
*/

        return res.reverse
      }
      ind += 1
    })
    return res.reverse
  }

}


