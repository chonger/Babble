import edu.mit.jwi.Dictionary
import edu.mit.jwi.morph.WordnetStemmer
import edu.mit.jwi.item.{POS,Pointer}
import java.io.File
import scala.collection.mutable.{HashMap,HashSet}
import scala.collection.JavaConversions._
import multitool._

abstract class ALexicon() {

  def getBase(s : String) : String
  def inVocab(s : String) : Boolean 
  def getClass(w : String, pos : String) : Int 

}

class DummyLexicon() extends ALexicon {

  def getBase(s : String) : String = ""
  def inVocab(s : String) : Boolean = false
  def getClass(w : String, pos : String) : Int = -1 

}

class Lexicon(val vocab : HashSet[String]) extends ALexicon {

  val base = "src/main/resources/"
  val snlg = new simplenlg.lexicon.XMLLexicon(getClass().getResource("/default-lexicon.xml").toURI)

  def getBase(s : String) = snlg.getWordFromVariant(s).getBaseForm()
  def inVocab(s : String) = vocab contains getBase(s) 
  
  def getClass(w : String, pos : String) : Int = -1

}

class Word2VecLexicon(cFile : String, words : HashSet[String]) extends Lexicon(words) {

  val clusts = new HashMap[String,Int]()

  io.Source.fromFile(cFile).getLines.foreach(l => {
    val p = l.trim.split("\\s")
    val w = p(0)
    val c = Integer.parseInt(p(1))
    clusts += w -> c
  })

  override def getClass(w : String, pos : String) : Int = clusts.getOrElse(w,-1)
}

import java.net.URL
class WNetLexicon(wnetURI : File, words : HashSet[String]) extends Lexicon(words) {
  
  def this(wnetPath : String, words : HashSet[String]) {
    this(new File(wnetPath),words)
  }

  def this(words : HashSet[String]) {
    this(new File(getClass().getClassLoader().getResource("dict").toURI),words)
  }
 
  val dict = new Dictionary(wnetURI)
  dict.open()
  val wClass = new SymbolTable() //cache wordnet glosses


  override def getClass(w : String, pos : String) : Int = {
    val wnPos = pos match {
      case "NN" => POS.NOUN
      case "NNS" => POS.NOUN
      case "VB" => POS.VERB
      case "VBZ" => POS.VERB
      case "VBG" => POS.VERB
      case "VBN" => POS.VERB
      case "VBP" => POS.VERB
      case "JJ" => POS.ADJECTIVE
      case "JJR" => POS.ADJECTIVE
      case "JJS" => POS.ADJECTIVE
      case "RB" => POS.ADVERB
      case "RBR" => POS.ADVERB
      case "RBS" => POS.ADVERB
      case _ => return -1
    }

    val iw = dict.getIndexWord(getBase(w),wnPos)
    if(iw == null)
      return -1
    val wIDS = iw.getWordIDs()
    if(wIDS.length > 0) {
      val ss = dict.getWord(wIDS(0)).getSynset()
      val hsets = ss.getRelatedSynsets(Pointer.HYPERNYM)
      
      if(hsets.length > 0) {
        val hs = hsets(hsets.length - 1)
        val hhs = dict.getSynset(hs)
        val g = hhs.getGloss()
        return wClass.add(g)
      } 
    } 

    -1
    
  }


}


