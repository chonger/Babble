import multitool.Shuffle


object Observation {

  def main(args : Array[String]) = {

    val lexicon = new Lexicon("/home/chonger/data/generate/vocab/A1_words.txt")

    //val tz = (DepNode.read("/home/chonger/data/generate/Proverbs.ptb").toList ::: DepNode.read("/home/chonger/data/generate/ptb/brown.txt").toList).toArray
    val tz = DepNode.read("/home/chonger/data/generate/simplewiki/newwiki2.ptb")
    
    Shuffle(tz)

    val gTz = tz.filter(t => {
      var good = true
      t.under().foreach(tt => {
        val ww = DepNode.words(tt.w)
        //println(ww + " - " + lexicon.inVocab(ww))
        if(ww != "ROOT" && !lexicon.inVocab(DepNode.words(tt.w).toLowerCase()))
          good = false
      })
      good
    })

    gTz.foreach(g => {
      println(g.sentence())
    })

    println(gTz.length + " completely good trees")

  }
/**
    var ind = 0

    var counts = List[List[Int]]()

    while(ind < tz.length) {

      val dg = new AllDeps()

      val ttz = tz.slice(0,ind + 1000)
      ind += 1000
      dg.addObservations(ttz)

      //we want the total and unique number of context -> outcomes

      val uniq = (0 /: dg.genProbs.iterator)(_ + _._2.size)
      val tot = (0 /: dg.genProbs.iterator)((a,b) => {
        a + (0 /: b._2.iterator)(_ + _._2.toInt)
      })

      dg.limit(lexicon,1)
      
      val vuniq = (0 /: dg.genProbs.iterator)(_ + _._2.size)
      val vtot = (0 /: dg.genProbs.iterator)((a,b) => {
        a + (0 /: b._2.iterator)(_ + _._2.toInt)
      })

      //also want to know --- how many context -> outcomes are deterministic? (entropy)

      counts ::= List(ind,tot,uniq,vtot,vuniq)
    }

    import java.io._

    val bw = new BufferedWriter(new FileWriter("/home/chonger/data/generate/obs.txt"))
    counts.foreach(x => bw.write(x.toArray.mkString("\t") + "\n"))
    bw.close()

  }
*/
}
