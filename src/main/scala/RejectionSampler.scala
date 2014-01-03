import multitool._
import scala.collection.mutable.{HashSet,HashMap}
import java.io._

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
    
    0.until(100000).foreach(i => {
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
