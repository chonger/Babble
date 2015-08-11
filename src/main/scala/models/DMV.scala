class DMVContext(val cType : Int, val c1 : (Int,Int), val c2 : (Int,Int)) extends Lookup {
  override def equals(any : Any) : Boolean = {
	any match {
	  case t : DMVContext => cType == t.cType && c1 == t.c1 && c2 == t.c2
	  case _ => false
	}	
  }
  override def hashCode() : Int = cType.hashCode() ^ c1.hashCode() ^ c2.hashCode()
  override def inLexicon(lexicon : Lexicon) : Boolean = {
    ((c1._1 == -1 || lexicon.inVocab(DepNode.words(c1._2).toLowerCase())) &&
    (DepNode.words(c2._2) == "ROOT" || lexicon.inVocab(DepNode.words(c2._2).toLowerCase())))
  }
  override def hasWord(w : String) : Boolean = {
    DepNode.words(c1._2) == w
  }
  override def save() : String = {
    cType + "\t" + List(c1,c2).map({case (pos,w) => {DepNode.postags(pos) + "\t" + DepNode.words(w)}}).toArray.mkString("\t")
  }
}

class DMVOutcome(val pos : Int, val w : Int) extends Lookup {
  //SAME AS NGRAM OUTCOME?
  override def equals(any : Any) : Boolean = {
	any match {
	  case t : DMVOutcome => pos == t.pos && w == t.w
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


class DMV extends Babbler[DMVContext,DMVOutcome] {

  import scala.collection.mutable.HashMap

  override def getObservations(t : DepNode) = {
    t.under().flatMap(n => {
      var cos = List[(DMVContext,DMVOutcome)]()
      
      val ls = ((-1,-1) :: n.left.map(x => (x.pos,x.w))).reverse
      val rs = ((-1,-1) :: n.right.map(x => (x.pos,x.w)).reverse).reverse
      
      val cur = (n.pos,n.w)

      var prev : (Int,Int) = (-1,-1)
      ls.foreach(l => {
        val c = new DMVContext(0,prev,cur)
        val o = new DMVOutcome(l._1,l._2)
        prev = l
        cos ::= (c,o) 
      })

      prev = (-1,-1)
      rs.foreach(l => {
        val c = new DMVContext(1,prev,cur)
        val o = new DMVOutcome(l._1,l._2)
        prev = l
        cos ::= (c,o) 
      })

      cos ::= (new DMVContext(-1,(-1,-1),cur),new DMVOutcome(-1,-1))

      cos
    })
  }

  override def implies(c : DMVContext, o : DMVOutcome) : List[DMVContext] = {
    c.cType match {
      case -1 => {
        List[DMVContext](new DMVContext(0,(-1,-1),(c.c2._1,c.c2._2)),new DMVContext(1,(-1,-1),(c.c2._1,c.c2._2)))
      }
      case i : Int => {
        if(o.pos == -1) {
          List[DMVContext]()
        } else {
          List[DMVContext](new DMVContext(c.cType,(o.pos,o.w),c.c2),new DMVContext(-1,(-1,-1),(o.pos,o.w)))
        }
      }  
    }
  }

  override def rootContext = {
    new DMVContext(-1,(-1,-1),(DepNode.postags.add("ROOT"),DepNode.words.add("ROOT")))
  }
    

  override def realize(k : CONode) : String = {    

    k.c.cType match {
      case -1 => {
        val w = DepNode.words(k.c.c2._2)
        if(w == "ROOT")
          realize(k.i(0)) + realize(k.i(1))
        else
          realize(k.i(0)) + " " + w + " " + realize(k.i(1))
      } 
      case 0 => {
        if(k.i.length > 0)
          realize(k.i(0)) + " " + realize(k.i(1))
        else
          ""
      }
      case 1 => {
        if(k.i.length > 0)
          realize(k.i(1)) + " " + realize(k.i(0))
        else
          ""
      }

    }
  }

  override def smoothC(c : DMVContext) : Any = c

  override def smoothO(c : DMVOutcome) : Any = c
  override def smoothImplies(c : Any, o : DMVOutcome) : List[Any] = {
    implies(c.asInstanceOf[DMVContext],o)
  }
}

