import multitool._
import scala.collection.mutable.{HashMap,HashSet}

object AllOutcome {
  def load(s : String) = {
    val ns = s.split("\\s").sliding(2,2)
    var ls = List[(Int,Int)]()
    var rs = List[(Int,Int)]()
    var doL = true
    ns.foreach(n => {
      if(n(0) == "-1")
        doL = false
      else {
        val pos = DepNode.postags.add(n(0))
        val w = DepNode.words.add(n(1))
        if(doL) ls ::= (pos,w) else rs ::= (pos,w)
      }
    })
    new AllOutcome(ls.reverse,rs.reverse)
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

  override def save() : String = {
    (left ::: ((-1,-1) :: right)).map({
      case (pos,w) => {
        if(pos == -1) 
          "-1\t-1" 
        else
          DepNode.postags(pos) + "\t" + DepNode.words(w)
      }
    }).toArray.mkString("\t")
  }
}

class FullSpine(lexicon : ALexicon) extends Babbler[NGramContext,AllOutcome] {

  import scala.collection.mutable.HashMap

  override def rootContext = {
    new NGramContext(List((DepNode.postags.add("ROOT"),DepNode.words.add("ROOT"))))
  }
  
  val examples = new HashMap[(NGramContext,AllOutcome),String]()

  override def getObservations(t : DepNode) = {

    val pMap = new HashMap[DepNode,DepNode]()
      
    t.under().foreach(n => {
      (n.left ::: n.right).foreach(nn => {
        pMap += nn -> n
      })
    })
    
    val ret = t.under().map(n => {
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


    val s = t.sentence()
    ret.foreach(x => {
      if(examples contains x) {
        //nothing
      } else 
        examples += x -> s
    })

    ret

  }

  def filt(plist : List[(Int,Int)]) : List[(Int,Int)] = plist

  override def implies(c : NGramContext, o : AllOutcome) : List[NGramContext] = {
    (o.left ::: o.right).map(x => {
      new NGramContext(filt(x :: c.prev))
    })
  }
  /**
  override def compose(k : NGramContext) : CONode = {
    val outcome = chooseSmooth(k)
    new CONode(k,outcome,outcome.left.map(x => compose(new NGramContext(filt(x :: k.prev)))),
               outcome.right.map(x => compose(new NGramContext(filt(x :: k.prev)))))
    /**
    new DepNode(cur._1,cur._2,
                outcome.left.map(x => compose(new NGramContext(filt(x :: k.prev)))),
                outcome.right.map(x => compose(new NGramContext(filt(x :: k.prev)))))
                */
  }  
  */
  override def realize(n : CONode) = {
    val numL = n.o.left.length
    val ls = n.i.take(numL)
    val rs = n.i.drop(numL)
    var w = DepNode.words(n.c.prev(0)._2)
    if(w == "ROOT") w = ""
    (ls.map(realize _).toArray.mkString(" ") + " " + w + " " + rs.map(realize _).toArray.mkString(" ")).trim
  }

  def recP(co : CONode,pref : String) : Unit = {

    val dCount = rawCounts.getOrElse(co.c,new HashMap[AllOutcome,Double]()).getOrElse(co.o,0)
    val aCount = (0.0 /: rawCounts.getOrElse(co.c,new HashMap[AllOutcome,Double]()).iterator)(_ + _._2)
    println(dCount + "\t" + aCount + "\t\t\t\t" + pref + co.c.toString + " [ ========> ] " + co.o.toString)
    println("EXAMPLE : " + examples.getOrElse((co.c,co.o),"None!"))
    co.i.foreach(cc => recP(cc, pref + "   "))
  }

  def depTree(n : CONode) : DepNode = {
    val numL = n.o.left.length
    val ls = n.i.take(numL)
    val rs = n.i.drop(numL)
    val cur = n.c.prev(0)
    new DepNode(cur._1,cur._2,ls.map(depTree _),rs.map(depTree _))
  }




  def classSmooth(npos : Int, nw : Int) : (Int,Int) = {
    if(nw == -1) {
      (npos,-1)
    } else {
      val w = DepNode.words(nw)
      val pos = DepNode.postags(npos)
      val h = lexicon.getClass(w,pos)
      if(h == -1)
        (npos,nw)
      else
        (npos,-h) //make sure it goes into a different set than the regular words!
    }
  }

  override def smoothC(c : NGramContext) : Any = c.prev.map({
    case (npos,nw) => {
      classSmooth(npos,nw)
    }
  })
  override def smoothO(o : AllOutcome) : Any = o.left.map(x => classSmooth(x._1,x._2)) ::: List((-5,-5)) ::: o.right.map(x => classSmooth(x._1,x._2))



  override def smoothImplies(a : Any, o : AllOutcome) : List[Any] = {
    a match {
      case cs : List[(Int,Int)] => {
        (o.left ::: o.right).map(x => {
          filt(classSmooth(x._1,x._2) :: cs)
        })
      }
    }
  }

}

class SpineDep(lexicon : ALexicon) extends FullSpine(lexicon) {

  def this(lexicon : ALexicon, f : String) = {
    this(lexicon)
    load(f,NGramContext.load,AllOutcome.load)
  }

  def this(f : String) = {
    this(new DummyLexicon(),f)
  }

  /**
   *
   *   Cuts upwards context chain at adj's, Nouns, Verbs, Prepositions, and root
   *   includes parent nonterminal of cutpoint
   * 
   */ 

  override def filt(pList : List[(Int,Int)]) : List[(Int,Int)] = {
/**
    var res = List[(Int,Int)]()
    var ind = 0
    var nHist = 1
    pList.foreach(x => {
      res ::= x
      val tag = DepNode.postags(x._1)
      val l1 = tag.charAt(0)
      if(tag == "PRP" || l1 == 'N' || l1 == 'V' || tag == "ROOT" || l1 == 'J') {
        
        if(ind < pList.length-1) { 
          res ::= (pList(ind+1)._1,-1)
          return res.reverse
        }

      }
      ind += 1
    })
    return res.reverse
    */

    var res = pList.take(2)
    if(pList.length > 2)
      res = ((pList(2)._1,-1) :: res.reverse).reverse
    res
  }

}

class SpineDepUS(lexicon : Lexicon) extends SpineDep(lexicon) {
  override def smoothC(c : NGramContext) : Any = c
  override def smoothO(o : AllOutcome) : Any = o
  override def smoothImplies(a : Any, o : AllOutcome) : List[Any] = {
    implies(a.asInstanceOf[NGramContext],o)
  }
}

