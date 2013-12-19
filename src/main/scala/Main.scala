import multitool._
import scala.collection.mutable.{HashSet,HashMap}

object Filter {

  def main(args : Array[String]) = {
    
    val st = new CFGSymbolTable()

    val tz = st.read("/home/chonger/data/generate/simplewiki/newwiki2.ptb").filter(t => {
      (true /: t.preterminals)((a,b) => a && st.syms(b.symbol) != "''") 
    })

    st.write("/home/chonger/data/generate/simplewiki/newwiki3.ptb",tz)

  }

}

object Main {

  def main(args : Array[String]) : Unit = {
    val lexicon = new Lexicon("/home/chonger/data/generate/vocab/A1_words.txt")
    
    /**
    val tz = (DepNode.read("/home/chonger/data/generate/Proverbs.ptb").toList ::: 
              DepNode.read("/home/chonger/data/PTB/train.txt").toList ::: 
              DepNode.read("/home/chonger/data/generate/ptb/brown.txt").toList).toArray

    val tz = DepNode.read("/home/chonger/data/generate/Proverbs.ptb")
    val tz = DepNode.read("/home/chonger/data/generate/ptb/small.txt")
    */

    //val tz = DepNode.read("/home/chonger/data/generate/simplewiki/simplewiki2.ptb")
    val tz = DepNode.read("/home/chonger/data/generate/simplewiki/newwiki3.ptb")
    //val tz = DepNode.read("/home/chonger/data/generate/simplewiki/med.ptb")
    //val tz = DepNode.read("/home/chonger/data/generate/simplewiki/small.ptb")
    //val tz = DepNode.read("/home/chonger/data/generate/simplewiki/debug.ptb")


    //Get a look at how many sentences in the data set are completely in vocab
    val orig = new HashSet[String]() ++ tz.par.map(x => x.seq().map(z => DepNode.words(z._2).toLowerCase())).filter(x => {
      (true /: x)((y,z) => y && lexicon.inVocab(z))
    }).map(_.toArray.mkString(" "))
    println(orig.size + " in vocab sentences")
    orig.iterator.slice(0,10).foreach(x => println(x))

    //val dg = new AllDepsL()
    val dg = new AllWordsF(lexicon)
    //val dg = new NGram(4)

    var gen : () => DepNode = dg.generate _

    var normal = false

    /**
    val m = new HashMap[Int,HashSet[(String,String)]]()
    
    tz.foreach(_.under.foreach(n => {
      val w = DepNode.words(n.w)
      if(lexicon.inVocab(w)) {
        val pos = DepNode.postags(n.pos)
        val h = lexicon.addHNym(w,pos)
        val k = (pos,w)
        val e = m.getOrElse(h,new HashSet[(String,String)]()) 
        e += k
        m += h -> e
      }
    }))
               
    
    println("HYPONYMS")
    m.iterator.foreach({
      case (i,ws) => {
        if(i == -1)
          println("NO HYPONYM")
        else
          println("HYPONYM : " + lexicon.hypos.strings(i))
        println(ws.map(x => x._1 + "/" + x._2).toArray.mkString(" "))
      }
    })
    */

    dg.addObservations(tz)
    dg.limitCounts(2)
    dg.em()

    if(normal) {
      dg.limit(lexicon,1)
      dg.prune()
      dg.normalize()
    } else {
      dg.trainSmooth(tz,lexicon)
      gen = dg.generateSmooth _
    }

    //ANALYSIS

    
    println("REGULAR")

    var fail = 0
    val gtz = 0.until(100).flatMap(i => {
      try {
        val t = gen()
        val s = t.sentence()
        if(orig contains s)
          println("!?! " + t.sentence())
        else
          println(t.sentence())
        if(t.under().length > 50) 
          List[DepNode]()
        else
          List(t)
      } catch {
        case e : Exception => {
          println(e)
          throw e
          fail += 1
          List[DepNode]()
        }
      }
    }).toArray

    println(fail + " fails")
    DepNode.makeTex("/home/chonger/vbshareX/made.tex",gtz)

    val dgi = new DepGramInv(dg,10)
    
    while(true) {
      val w = readLine().trim
      val poss = dgi.allContexts.filter(_.hasWord(w)).toSeq
      if(poss.length == 0) {
        println("No possible start contexts for " + w)
      } else {
        0.until(100).foreach(i => {
          println(dgi.generateUp(w).sentence())
        })
      }
    }
    
  }
   
}
