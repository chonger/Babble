import scala.collection.mutable.{HashMap,HashSet}
import multitool._

class LogRegContext(val item : RefWrapper, 
                    val chains : HashMap[RefWrapper,List[NonTerminalNode]],
                    val parentChain : HashMap[RefWrapper,RefWrapper],
                    val lexes : HashMap[RefWrapper,String])

class LogRegLex(val st : CFGSymbolTable, baseDir : String) extends BabbleLex {
  
  //we need a classifier for each pt sym
  val models = new HashMap[Int,MalletClass[String]]()

  val extractor = new LexExtractor(st)

  //load models!
  import java.io.File
  val baseFile = new File(baseDir)
  baseFile.list().foreach(x => {
    val sym = st.syms.add(x.drop(10))
    println(x.drop(10))
    models += sym -> MalletClass.load[String](baseDir + x)
  })

  override def pickWord(item : RefWrapper, 
               chains : HashMap[RefWrapper,List[NonTerminalNode]],
               parentChain : HashMap[RefWrapper,RefWrapper],
               lexes : HashMap[RefWrapper,String]) : String = {
    
    //print("Lexing " + item.n.toString(st))

    //val s = item.n.symbol

    //val sStr = st.syms(item.n.symbol).split("-").reverse.drop(1).reverse.mkString("-")
    val sStr = st.syms(item.n.symbol)
    val s = st.syms.add(sStr)
    //print("Lexing " + sStr)

    if(sStr == "-RRB-")
      return ")"

    if(sStr == "-LRB-")
      return ")"
    
    if(sStr == ":")
      return ":"

    try {
      val context = new LogRegContext(item,chains,parentChain,lexes)
      val fs = extractor.extractBinary(context)
      //fs.foreach(f => println(f))
      val predictions = models(s).predict(fs)
      //predictions.foreach(p => println(p))
      //val w = predictions.sortWith(_._2 > _._2)(0)._1

      val w = Babble.sample(predictions)
      //println(" --> " + w)
      w
    } catch {
      case _ => {
        println("FAIL - on " + sStr) 
        "!!!!!!"
      }
    }
  }

}

    
class LexExtractor(st : CFGSymbolTable) extends FExtractor[LogRegContext,String] {
  override def extract(c : LogRegContext) : List[String] = {
    var fs = List[String]()
    
    def baseS(n : NonTerminalNode) : String = {
      st.syms(n.symbol).split("-")(0)
    }

    val pHead = c.parentChain.getOrElse(c.item,null)
    val myChain = c.chains(c.item)

    if(pHead == null)
      fs ::= "PHEAD:NULL"
    else {
      //fs ::= "PHEAD:" + pHead.n.toString(st).replaceAll("\\s","_")
      fs ::= "PSYM:" +  baseS(pHead.n)
      fs ::= "PLEX:" + c.lexes(pHead).toLowerCase

      c.parentChain.iterator.foreach(x => {
        if(x._2 == pHead && x._1 != c.item) {
          fs ::= "SISTER:" + baseS(x._1.n)
        }
      })
      
    }

    fs ::= "FULLSYM:" + st.syms(c.item.n.symbol)

    var path = myChain.map(x => baseS(x)).toArray
    
    fs ::= "PATH:" + path.mkString("_")

    fs ::= "PATHTOP:" + path.reverse(0)

    c.parentChain.iterator.foreach(x => {
      if(x._2 == c.item) {
        fs ::= "HASKID:" + baseS(x._1.n)
      }
    })
    
    /**
    println(fs.toArray.mkString("\n"))
    readLine()
    */

    fs
  }
}


object LogRegTrain {

  val base = "/home/chonger/data/generate/"

  import java.util.logging.{LogManager,Level}

  import edu.mit.jwi.Dictionary
  import edu.mit.jwi.morph.WordnetStemmer
  import java.io.File

  def main(args : Array[String]) : Unit = {

    val wnetPath = base + "wordnet/WordNet-3.0/dict"
    
    val lexicon = new simplenlg.lexicon.XMLLexicon(base + "simplenlg/simplenlg-v44/res/default-lexicon.xml")

    val dict = new Dictionary(new File(wnetPath))
    dict.open()
    val stemmer = new WordnetStemmer(dict)

    //LogManager.getLogManager().getLogger("optimize").setLevel(Level.OFF)

    val vocab = new HashSet[String]()
    
    io.Source.fromFile(base + "vocab/A1_words.txt").getLines.foreach(x => {
      /**
      val stems = stemmer.findStems(x,null).toArray
      println(x + " - " + stems.mkString(" "))
      val www = dict.getIndexWord(x,null)
      println(www.getLemma())
      */
      /**
      println(x)
      val we = lexicon.getWord(x)
      println(we)
      readLine()
      */
      vocab += x.trim()
    })
    

    val st = new CFGSymbolTable()
    val tz = (st.read(base + "Proverbs_ann.ptb") ::: st.read(base + "ptb/brown_ann.txt")).map(x => {
    
      def rrr(n : NonTerminalNode) : List[NonTerminalNode] = {
        n match {
          case in : ProtoNode => {
            if(st.syms(in.symbol).indexOf("@") == 0)
              in.children.flatMap(x => rrr(x))
            else
              List(new ProtoNode(in.symbol,in.children.flatMap(x => rrr(x))))
          }
          case ptn : PreTerminalNode => List(ptn)
        }
      }
      
      val rootz = rrr(x.root)
      new ParseTree(rootz(0))
    })
    //val tz = st.read(base + "ptb/brown_ann.txt")
    val extractor = new LexExtractor(st)

    val examples = new HashMap[String,List[(String,LogRegContext)]]()

    tz.foreach(t => {
      val hf = new HeadFinder(st,t)
      val (chains,parentChain) = BabbleLex.getChains(st,t,hf)
      val lexes = new HashMap[RefWrapper,String]

      chains.iterator.foreach({
        case (ptn,ch) => {
          val s = st.terms(ptn.n.asInstanceOf[PreTerminalNode].kid.terminal)
          if(s != "UNK") {
            lexes(ptn) = s
          }
        }
      })

      chains.keys.foreach(k => {
        var symP = st.syms(k.n.symbol).split("-")
        //val sym = symP.slice(0,symP.length - 1).mkString("-")
        val sym = symP.mkString("-")


        //var sym = st.syms(k.n.symbol)
        val word = st.terms(k.n.asInstanceOf[PreTerminalNode].kid.terminal)    
        val base = lexicon.getWordFromVariant(word).getBaseForm()

        
        //if(sym == "VBZ")
        //  println(word + "/" + base)


        if(vocab.contains(base)) {
          val context = new LogRegContext(k,chains,parentChain,lexes)   
          var entry = examples.getOrElse(sym,List[(String,LogRegContext)]())
          entry ::= (word,context)
          examples += sym -> entry
        }
      })
    })

    println("Examples loaded")

    //Examples are loaded

    examples.iterator.foreach({
      case (sym,exes) => {
        println("Learning model for " + sym)

        val exes2 = exes.groupBy(_._1).filter(_._2.size >= 10).iterator.toList.flatMap(_._2)
        
        println(exes2.groupBy(_._1).size + " possible lexicalizations")
        exes2.groupBy(_._1).foreach(x => {
          println(x._1 + " - " + x._2.size)
        })


        val model = new MalletClass[String]()
        model.train(exes2.map({
          case (l,c) => (l,extractor.extractBinary(c))
        }))
        val filename = base + "lex/model_for_" + sym
        MalletClass.save(filename,model)
        //readLine()
      }
    })

  }

}

