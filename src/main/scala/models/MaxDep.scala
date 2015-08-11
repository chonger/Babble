
object MaxContext {
  def load(s : String) = {
    if(s == "")
      new MaxContext(Nil)
    else {
      val ns = s.split("\\|").sliding(2,2)
      new MaxContext(ns.map(n => {
        //println(n.toArray.mkString("\n"))
        (AllOutcome.load(n(0)),n(1).toInt)
      }).toList)
    }
  }
}

class MaxContext(val os : List[(AllOutcome,Int)]) extends Lookup {
  override def equals(any : Any) : Boolean = {
	any match {
	  case t : MaxContext => os == t.os
	  case _ => false
	}	
  }
  override def hashCode() : Int = os.hashCode()

  override def inLexicon(lexicon : Lexicon) : Boolean = {
    (true /: os)((a,b) => {
      a && b._1.inLexicon(lexicon)
    })
  }

  override def toString() : String = {
    os.map({
      case (o,i) => o.toString() + " [" + i + "]"
    }).toArray.mkString(" -> ")
  }

  override def hasWord(w : String) : Boolean = {
    if(os.length == 0) return false
    val (o,i) = os(0)
    val item = if(i > 0) o.right(i-1) else o.left(-(i+1))
    DepNode.words(item._2).toLowerCase() == w.toLowerCase()
  }

  override def save() : String = {
    os.map({
      case (o,i) => {
        o.save() + "|" + i
      }
    }).toArray.mkString("|")
  }
}


class MaxDep(var lexicon : Lexicon) extends Babbler[MaxContext,AllOutcome] {

  def this(lexicon : Lexicon, f : String) = {
    this(lexicon)
    load(f,MaxContext.load,AllOutcome.load)
  }

  def this(f : String) = {
    this(null,f)
  }

  if(lexicon == null) {
    import scala.collection.mutable.HashSet
    def getVocab(wordsFile : String) : HashSet[String] = {
      val vocab = new HashSet[String]()    
      io.Source.fromFile(wordsFile).getLines.foreach(x => {
        vocab += x.trim().toLowerCase()
      })
      vocab
    }
    lexicon = new WNetLexicon(getVocab("src/main/resources/A1_words.txt"))
  }

  import scala.collection.mutable.HashMap

  override def rootContext = {
    new MaxContext(List[(AllOutcome,Int)]())
  }
  
  val examples = new HashMap[(MaxContext,AllOutcome),String]()

  override def getObservations(t : DepNode) = {

    val pMap = new HashMap[DepNode,DepNode]()
    val cMap = new HashMap[DepNode,MaxContext]()
      
    t.under().foreach(n => {
      (n.left ::: n.right).foreach(nn => {
        pMap += nn -> n
      })
    })

    var ret = List[(MaxContext,AllOutcome)]()

    def record(sofar : MaxContext, n : DepNode) : Unit = {
      val outcome = new AllOutcome(n.left.map(x => (x.pos,x.w)),n.right.map(x => (x.pos,x.w)))
      ret ::= (sofar,outcome)
      n.left.zipWithIndex.foreach({
        case (nn,i) => record(new MaxContext((outcome,-(i+1)) :: sofar.os),nn) 
      })
      n.right.zipWithIndex.foreach({
        case (nn,i) => record(new MaxContext((outcome,(i+1)) :: sofar.os),nn) 
      })
    }
    record(rootContext,t)

    val s = t.sentence()
    ret.foreach(x => {
      if(examples contains x) {
        //nothing
      } else 
        examples += x -> s
    })

    /**
    ret.foreach(x => {
      println()
      println(x._1)
      println(x._2)
    })
    readLine()
    */
    ret

  }

  override def implies(c : MaxContext, o : AllOutcome) : List[MaxContext] = {
    0.until(o.left.length).map(i => new MaxContext((o,-(i+1)) :: c.os)).toList :::
    0.until(o.right.length).map(i => new MaxContext((o,(i+1)) :: c.os)).toList
  }
  
  override def realize(n : CONode) = {
    if(n.c.os.length == 0) //root!
      realize(n.i(0))
    else {
      val numL = n.o.left.length
      val ls = n.i.take(numL)
      val rs = n.i.drop(numL)
      val lastO = n.c.os(0)._1
      val ind = n.c.os(0)._2
      val pw = if(ind < 0) lastO.left(-(ind+1)) else lastO.right(ind-1)
      var w = DepNode.words(pw._2)
      (ls.map(realize _).toArray.mkString(" ") + " " + w + " " + rs.map(realize _).toArray.mkString(" ")).trim
    }
  }

  /**
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
  */


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

  override def smoothC(c : MaxContext) : Any = c.os.map(x => {
    (smoothO(x._1),x._2)
  })
  override def smoothO(o : AllOutcome) : Any = o.left.map(x => classSmooth(x._1,x._2)) ::: List((-5,-5)) ::: o.right.map(x => classSmooth(x._1,x._2))

  override def smoothImplies(a : Any, o : AllOutcome) : List[Any] = {
    val so = smoothO(o)
    a match {
      case sos : List[(List[(Int,Int)],Int)] => {
        0.until(o.left.length).map(i => (so,-(i+1)) :: sos).toList :::
        0.until(o.right.length).map(i => (so,(i+1)) :: sos).toList
      }
    }
  }

}

