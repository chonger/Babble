import scalafrontend._
import multitool._
import scala.collection.mutable.{HashSet,HashMap}

object Ingest {

  def ingestFile(filE : String, st : CFGSymbolTable, args : HashMap[String,String]) : Array[ParseTree] = {
      
    val fin = io.Source.fromFile(filE)
    
    val text = args.getOrElse("SENTD","FULL") match {
      case "FULL" => OpenNLP.sentenceDetect(fin.getLines.toArray.mkString(" ").replaceAll("\\s+"," "))
      case "PART" => fin.getLines.toArray.flatMap(x => OpenNLP.sentenceDetect(x))
      case "NONE" => fin.getLines.toArray
    }

    println("- Parsing -")
    println(". = sucessful parse")
    println("! = parse failure")

    var c = 0
    print("0\t\t")
    val treez = text.flatMap(s => {
      if(c > 0 && c % 50 == 0) {
        println()
        print(c + "\t\t")
      }
      c += 1
      try {
        val r = List(Stanford.parse(s,st))
        print(".")
        r
      } catch {
        case r : Exception => {
          print("!")
          List[ParseTree]()
        }
      }
    }).toArray

    println()
    fin.close()
    treez
  }
  
  def sfilter(tz : Array[ParseTree],st : CFGSymbolTable) = {
    tz.par.filter(t => {
      val topnode = (t.root.children)(0).asInstanceOf[NonTerminalNode]
      val tSym = st.syms(topnode.symbol)
      if(tSym.charAt(0) == 'S') {
        val k = topnode.children.map(n =>  st.syms(n.asInstanceOf[NonTerminalNode].symbol))
        (k contains "NP") && (k contains "VP")
      } else {
        false
      }
    }).toArray
  }

  def parseThenSave(filE : String, output : String,args : HashMap[String,String]) = {
    val st = new CFGSymbolTable()
    val sents = ingestFile(filE,st,args)
    import java.io._
    val bw = new BufferedWriter(new FileWriter(output))
    sents.foreach(s => bw.write(s.fString(st) + "\n"))
    bw.close()
  }

  def loadThenBuild(filE : String, output : String,args : HashMap[String,String]) = {
    val st = new CFGSymbolTable()
    val sents = io.Source.fromFile(filE).getLines.map(x => {
      st.growTree(x.trim)
    }).toArray
    build(sents,output,st,args)
  }

  def parseThenBuild(filE : String, output : String,args : HashMap[String,String]) = {
    val st = new CFGSymbolTable()
    val sents = ingestFile(filE,st,args)
    build(sents,output,st,args)
  }
                
  def build(sents : Array[ParseTree], output : String, st : CFGSymbolTable, args : HashMap[String,String]) : Unit = {

    println("Building a Babbler")
    
    import scala.collection.mutable.{HashSet,HashMap}
    
    var vocabType = args.getOrElse("vocabType","COUNT")
    var vocabArg = args.getOrElse("vocabArg","3000")
    var smooth = args.getOrElse("smooth","1").toInt
    var fullEst = args.contains("fullEst")
    var filter = args.getOrElse("filter","NONE")

    if(args contains "writeOrig") {
      val orig = new HashSet[String]() ++ sents.map(x => x.sentence(st).toLowerCase())
      import java.io._
      val bw = new BufferedWriter(new FileWriter(args("writeOrig")))
      orig.iterator.foreach(x => bw.write(x + "\n"))
      bw.close()
    }

    var inputSents = filter match {
      case "NONE" => sents
      case "SNPVP" => sfilter(sents,st)
    }

    val dtreez = sents.map(x => DepNode.readL(x,st)).toArray

    val vocab = new HashSet[String]()
    vocabType match {
      case "COUNT" => {
        val counts = new HashMap[String,Int]()
        dtreez.foreach(_.under().foreach(x => {
          val w = DepNode.words(x.w)
          val e = counts.getOrElse(w,0) + 1
          counts += w -> e
        }))

        counts.iterator.toArray.filter(_._1 != "ROOT").sortWith(_._2 > _._2).slice(0,vocabArg.toInt).foreach(x => {
          vocab += x._1
        })
      }
      case "FILE" => {
        val f = io.Source.fromFile(vocabArg)
        vocab ++= f.getLines.map(x => x.trim())
        f.close()
      }
    }

    val lexicon = new Lexicon(vocab)
    val dg = new SpineDep(lexicon)

    dg.addObservations(dtreez)
    dg.limitCounts(smooth) //setting to 1 does not limit anythin
    dg.em(20)
    dg.vocabPrune(lexicon)
    dg.setProbs(fullEst)
    dg.save(output)

    println("Babbler saved to " + output)

  }

}
