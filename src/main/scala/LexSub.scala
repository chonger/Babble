import multitool._


object LexSub {


  def main(args : Array[String]) : Unit = {

    val lexicon = new Lexicon("/home/chonger/data/generate/vocab/A1_words.txt")

    val st = new CFGSymbolTable()

    val tz = st.read("/home/chonger/data/generate/Proverbs_ann.ptb")

    0.until(10).foreach(x => {
      val t = tz(Babble.rando.nextInt(tz.length))
      println("ORIGINAL")
      println(t.sentence(st))
      t.preterminals.foreach(x => {
        val w = st.terms(x.kid.terminal)
        if(!lexicon.inVocab(w))
          println(x.toString(st))
      })
    })

  }

}
