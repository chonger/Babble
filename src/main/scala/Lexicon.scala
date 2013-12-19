
import edu.mit.jwi.Dictionary
import edu.mit.jwi.morph.WordnetStemmer
import edu.mit.jwi.item.{POS,Pointer}
import java.io.File
import scala.collection.mutable.{HashMap,HashSet}
import scala.collection.JavaConversions._
import multitool._

class Lexicon(wordsFile : String) {

  val base = "/home/chonger/data/generate/"
  val snlg = new simplenlg.lexicon.XMLLexicon(base + "simplenlg/simplenlg-v44/res/default-lexicon.xml")
  val wnetPath = base + "wordnet/WordNet-3.0/dict"
  val dict = new Dictionary(new File(wnetPath))
  dict.open()

  val vocab = new HashSet[String]()
    
  io.Source.fromFile(wordsFile).getLines.foreach(x => {
    vocab += x.trim().toLowerCase()
  })


  def getBase(s : String) = snlg.getWordFromVariant(s).getBaseForm()
  
  def inVocab(s : String) = vocab contains getBase(s) 

  val hypos = new SymbolTable()
  
  def addHNym(w : String, pos : String) : Int = {
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
        /**
         hhs.getWords().foreach(hw => {
         println(hw.getLemma())
         })
         */ 
        return hypos.add(g)
      } 
    } 

    -1
    
  }

}

object Lexicon {

  def main(args : Array[String]) = {

    val lex = new Lexicon("/home/chonger/data/generate/vocab/A1_words.txt")

    val ws = List("dogs","dog","school","shampoo","cat","fish","rope","soap")

    ws.foreach(w => {
      println()
      println("Word : " + w)
      println("Base : " + lex.getBase(w))
      println("HYPONYMS")
      lex.dict.getIndexWord(lex.getBase(w),POS.NOUN).getWordIDs().slice(0,1).toList.foreach(x => {
        val ss = lex.dict.getWord(x).getSynset()
        ss.getRelatedSynsets(Pointer.HYPERNYM).foreach(hs => {
          val hhs = lex.dict.getSynset(hs)
          println("! " + hhs.getGloss())
          hhs.getWords().foreach(hw => {
            println(hw.getLemma())
          })
        })
      })

    })

  }

}

