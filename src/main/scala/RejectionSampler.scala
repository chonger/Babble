import multitool._
import scala.collection.mutable.{HashSet,HashMap}
import java.io._

object Wordlister {
  
  def main(args : Array[String]) : Unit = {
    val st = new CFGSymbolTable()
    val tz = st.read("/home/chonger/data/generate/simplewiki/newwiki3.ptb")
    var bw = new BufferedWriter(new FileWriter("/home/chonger/data/generate/ex/sents.txt"))
    tz.foreach(t => {
      bw.write(t.sentence(st) +"\n")
    })
    bw.close()
  }
  
  def poo() : Unit = {
    val lexicon = new Lexicon("/home/chonger/data/generate/vocab/A1_words.txt")
    val tz = DepNode.read("/home/chonger/data/generate/simplewiki/newwiki3.ptb")

    val goodW = new HashSet[String]()
    tz.foreach(t => {
      t.seq().map(z => DepNode.words(z._2).toLowerCase()).foreach(w => {
        if(lexicon.inVocab(w))
          goodW += w
      })
    })
    
    var bw = new BufferedWriter(new FileWriter("/home/chonger/data/generate/rejects/goodW.txt"))
    goodW.iterator.foreach(w => {
      bw.write(w + "\n")
    })
    bw.close() 
  }
}


object CompareSmooth {

  def main(args : Array[String]) : Unit = {

    var xtype = ""

    while(xtype == "") {
      println("Which one? (U/S)")
      val s = readLine().trim
      if(s == "U" || s == "S")
        xtype = s
    }

    //US - 1715

    val lexicon = new Lexicon("/home/chonger/data/generate/vocab/A1_words.txt")
    val tz = DepNode.read("/home/chonger/data/generate/simplewiki/newwiki3.ptb")
    //val tz = DepNode.read("/home/chonger/data/generate/simplewiki/med.ptb")
    
    val dg = if(xtype == "S") new AllWordsF(lexicon) else new AllWordsUS(lexicon)
    dg.addObservations(tz)
    dg.limitCounts(2)
    dg.em()
    dg.firstPass(lexicon)
    dg.setProbs()

    var bw = new BufferedWriter(new FileWriter("/home/chonger/data/generate/rejects/" + xtype + ".txt"))
    0.until(100).foreach(i => {
      val t = dg.generateSmooth()
      val s = t.sentence()
      bw.write(s + "\n")
    })
    bw.close()

    val ss = new HashSet[String]()
    0.until(100000).foreach(i => {
      val t = dg.generateSmooth()
      val s = t.sentence()
      ss += s
    })
    println("Uniq in 100000 - " + ss.size)

  }

}


object EvalApprox {

  def main(args : Array[String]) : Unit = {

    val NUMS = 300

    val lexicon = new Lexicon("/home/chonger/data/generate/vocab/A1_words.txt")
    val tz = DepNode.read("/home/chonger/data/generate/simplewiki/med.ptb")
    val dg = new AllWordsF(lexicon)
    dg.addObservations(tz)
    dg.limitCounts(2)
    dg.em()

    println("REJECTION SAMPLER")
    var bw = new BufferedWriter(new FileWriter("/home/chonger/data/generate/rejects/EX2_RS.txt"))
    var nGen = 0
    while(nGen < NUMS) {
      try {
        val t = dg.compose(dg.rootContext,dg.chooseBasic)
        val ws = t.seq().map(z => DepNode.words(z._2).toLowerCase())
        if((true /: ws)((y,z) => y && lexicon.inVocab(z))) {
          val s = ws.mkString(" ")
          bw.write(s + "\n")
          nGen += 1
        } 
      } catch {
        case e : Exception => {
          //do nothing
        }
      }
    }
    bw.close()

    println("ESTIMATED SAMPLER")
    bw = new BufferedWriter(new FileWriter("/home/chonger/data/generate/rejects/EX2_EST.txt"))
    dg.firstPass(lexicon)
    dg.setProbs()
    0.until(NUMS).foreach(i => {
      val t = dg.generateSmooth()
      val s = t.sentence()
      bw.write(s + "\n")
    })
    bw.close()

    println("UNIFORM SAMPLER")
    bw = new BufferedWriter(new FileWriter("/home/chonger/data/generate/rejects/EX2_UNI.txt"))
    dg.probs.foreach({
      case (c,os) => {
        var v = 1.0 / os.size
        os.iterator.foreach(x => {
          os += x._1 -> v
        })
      }
    })
    0.until(NUMS).foreach(i => {
      val t = dg.generateSmooth()
      val s = t.sentence()
      bw.write(s + "\n")
    })
    bw.close()
  }

}

object VocabRejector {

  def main(args : Array[String]) : Unit = {

    val lexicon = new Lexicon("/home/chonger/data/generate/vocab/A1_words.txt")
    val tz = DepNode.read("/home/chonger/data/generate/simplewiki/newwiki3.ptb")
    val dg = new AllWordsF(lexicon)
    dg.addObservations(tz)
    dg.limitCounts(2)
    dg.em()

    var fails = 0
    var rejects = 0
    var counts = new HashMap[String,Int]()
    0.until(4000).foreach(i => {
      if(i % 1000 == 0)
        println(i + " F : " + fails + " R : " + rejects)
      try {
        val t = dg.compose(dg.rootContext,dg.chooseBasic)
        val ws = t.seq().map(z => DepNode.words(z._2).toLowerCase())
        if((true /: ws)((y,z) => y && lexicon.inVocab(z))) {
          val s = ws.mkString(" ")
          val e = counts.getOrElse(s,0) + 1
          counts += s -> e
        } else {
          rejects += 1
        }  
      } catch {
        case e : Exception => {
          fails += 1
        }
      }
    })

    println("FINAL F : " + fails + " R : " + rejects)

    var bw = new BufferedWriter(new FileWriter("/home/chonger/data/generate/rejects/RS1.txt"))
    counts.iterator.foreach(x => {
      bw.write(x._2 + "\t" + x._1 + "\n")
    })
    bw.close()

  }

}
