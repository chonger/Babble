import scala.collection.mutable.{HashMap,HashSet}

class Active(sdepF : String, goldF : String, blackF : String) {

  type CONode = CON[NGramContext,AllOutcome]

  val goldMCO = try {
    Active.loadMCO(goldF)
  } catch {
    case e : java.io.FileNotFoundException => {
      println("Initializing goldMCO (No gold file found at " + goldF + ")")
      new HashMap[MaxContext,HashSet[AllOutcome]]()
    }
  }
  val blackMCO = try {
    Active.loadMCO(blackF)
  } catch {
    case e : java.io.FileNotFoundException => {
      println("Initializing blackMCO (No black file found at " + blackF + ")")
      new HashMap[MaxContext,HashSet[AllOutcome]]()
    }
  }

  println("GOLD MCOS - " + (0 /: goldMCO.iterator)(_ + _._2.size))
  println("BLACK MCOS - " + (0 /: blackMCO.iterator)(_ + _._2.size))

  val sdep = new SpineDep(sdepF)

  def generate() = compose(sdep.rootContext,new MaxContext(Nil),useBlack)
  def generateEx(mc : MaxContext, outcome : AllOutcome) = {
    var cur = sdep.rootContext
    val chain = mc.os.reverse.map({
      case (o,i) => {
        val im = sdep.implies(cur,o)
        val ret = (cur,o,i)
          cur = im(i)
        ret
      }
    }).toList ::: List((cur,outcome,0))  
    composeChain(chain,new MaxContext(Nil),useGold)
  }
  
  def composeChain(chain : List[(NGramContext,AllOutcome,Int)], sofar : MaxContext, chooser : (NGramContext,MaxContext) => AllOutcome) : CONode = {
    val (c,outcome,ii) = chain(0)
    var nextChain = chain.drop(1)
    val im = sdep.implies(c,outcome)
    
    if(nextChain.length > 0) {
      val next = 0.until(im.length).map(jj => {
        val nextMC = new MaxContext((outcome,jj) :: sofar.os)
        if(jj == ii) 
          composeChain(nextChain,nextMC,chooser) 
        else 
          compose(im(jj),nextMC,chooser)
      }).toList
      new CONode(c,outcome,next)
    } else {
      val next = 0.until(im.length).map(jj => {
        val nextMC = new MaxContext((outcome,jj) :: sofar.os)
        compose(im(jj),nextMC,chooser)
      }).toList
      new CONode(c,outcome,next)
    }
  }
  
  def useGold(c : NGramContext, sofar : MaxContext) : AllOutcome = {
    if(goldMCO contains sofar) {
      //println("USED GOLD")
      //println(sofar)
      val poss = goldMCO(sofar).iterator.toArray
      poss(Babble.rando.nextInt(poss.length))
    } else {
      useBlack(c,sofar)
    }
  }

  def useBlack(c : NGramContext, sofar : MaxContext) : AllOutcome = {
    if(blackMCO contains sofar) {
      val blacklist = blackMCO(sofar)
      val poss = sdep.probs(c).filter(x => !(blacklist contains x._1)) 
      sdep.normalize(poss)
      Babble.sample(poss.iterator)
    } else {
      sdep.chooseSmooth(c)
    }
  }
  
  def compose(c : NGramContext, sofar : MaxContext, chooser : (NGramContext,MaxContext) => AllOutcome) : CONode = {
    val outcome = chooser(c,sofar)
    val im = sdep.implies(c,outcome)
    val next = 0.until(im.length).map(jj => {
      val nextMC = new MaxContext((outcome,jj) :: sofar.os)
      compose(im(jj),nextMC,chooser)
    }).toList
    new CONode(c,outcome,next)
  }
    
  def getCoarse(mc : MaxContext) : NGramContext = {
    var x = (mc.os.map({
      case (o,i) => {
        val all = (o.left ::: o.right).toArray
        all(i)
      }
    }) ::: sdep.rootContext.prev).slice(0,3).toArray
    if(x.length == 3) {
      val y = x(2)
      x(2) = (y._1,-1)
    }
    new NGramContext(x.toList)
  }
  
  def addBlacklist(mc : MaxContext, o : AllOutcome) : Unit = {
    val curBL = blackMCO.getOrElseUpdate(mc,new HashSet[AllOutcome]()) 
    curBL += o

    //if the blacklist removes all outcomes, then this max context is not ok
    val coarse = getCoarse(mc)
    val outs = sdep.probs(coarse)
    if(outs.size == curBL.size) { //all outcomes are blacklisted for this MC
      blackMCO -= mc
      val oneUp = new MaxContext(mc.os.drop(1))
      val oo = mc.os(0)._1
      println("Passing blame up")
      println(oneUp)
      println(oo)
      addBlacklist(oneUp,oo)
    }

  }

  def extractMCO(n : CONode, dof : (MaxContext,AllOutcome) => Unit) : Unit = {
    extractMCO_r(new MaxContext(Nil),n,dof)
  }
  def extractMCO_r(soFar : MaxContext,n : CONode, dof : (MaxContext,AllOutcome) => Unit) : Unit = {
    dof(soFar,n.o)
    0.until(n.i.length).foreach(ii => {
      val newSoFar = new MaxContext((n.o,ii) :: soFar.os) 
      extractMCO_r(newSoFar,n.i(ii),dof)
    })
  }
  def canGenerate(n : CONode) : Boolean = {
    var mco = List[(MaxContext,AllOutcome)]()
    def mydo(a : MaxContext, b : AllOutcome) = {mco ::= (a,b)}
    extractMCO(n,mydo)
    (true /: mco)((a,b) => {
      a && !((blackMCO contains b._1) && (blackMCO(b._1) contains b._2))
    })
  }
  def allGold(n : CONode) : Boolean = {
    var mco = List[(MaxContext,AllOutcome)]()
    def mydo(a : MaxContext, b : AllOutcome) = {mco ::= (a,b)}
    extractMCO(n,mydo)
    (true /: mco)((a,b) => {
      a && (goldMCO contains b._1) && (goldMCO(b._1) contains b._2)
    })
  }

  val blackstack = new HashMap[String,CONode]()

  def commitAll(t : CONode) = {
    var mco = List[(MaxContext,AllOutcome)]()
    def mydo(a : MaxContext, b : AllOutcome) = {mco ::= (a,b)}
    extractMCO(t,mydo)
    mco.foreach({
      case (a,b) => {
        goldMCO.getOrElseUpdate(a,new HashSet[AllOutcome]()) += b
      }
    })
    println("# GoldCs = " + (0 /: goldMCO.iterator)(_ + _._2.size))
  }


  var original : String = ""
  var toInvestigate = List[(MaxContext,CONode,Array[(CONode,String)])]()
  var investigating : Boolean = true

  def startInvestigation(t : CONode) = {
    original = sdep.realize(t)
    toInvestigate = List[(MaxContext,CONode,Array[(CONode,String)])]()    
    investigating = true
    preinvestigate(new MaxContext(Nil),t)
    investigate()
  }
  
  def addkids(mc : MaxContext, n : CONode) : Unit = {
    0.until(n.i.length).foreach(ii => {
      val newSoFar = new MaxContext((n.o,ii) :: mc.os) 
      preinvestigate(newSoFar,n.i(ii))
    })
  }
  
  def blame(mc : MaxContext, n : CONode) : Unit = {
    println()
    println("BLAME")
    println(mc + " -------------> " + n.o)
    println()
    investigating = false
    addBlacklist(mc,n.o)        
    getSentence = newSentence
  }

  /**
   *
   *  Fills up toInvestigate, but can also place blame and end the investigation
   */ 
  def preinvestigate(mc : MaxContext, n : CONode) : Unit = {
    
    if(!investigating)
      return

    //if this mc -> o turns out ok, we will all this to investigate its kids
    
    /**
     *  AUTOMATIC (Assumtion based) EVALUATION
     *
     */ 
    //if there are 0 or 1 outcomes, assume it's ok
    if(mc.os.length < 2) {
      addkids(mc,n)
      return
    }
    //if its in the gold list
    if(goldMCO contains mc) {
      if(goldMCO(mc) contains n.o) {
        addkids(mc,n)
        return
      }
    }
    val ref = new HashMap[String,CONode]()
    val made = new HashMap[String,Int]()
    0.until(2000).foreach(xx => {
      val tt = generateEx(mc,n.o)
      val ss = sdep.realize(tt)
      if(ss != original) {
        if(made contains ss) {
          val e = made(ss) + 1
          made += ss -> e
        } else {
          made += ss -> 1
          ref += ss -> tt
        }
      }
    })
    val trees = made.iterator.toArray.sortWith(_._2 > _._2).filter(x => !(blackstack contains x._1)).map(x => (ref(x._1),x._1))
    
    if(trees.length == 0) { //the only sentence we generated was the original
      println("only regenerates original sentence")
      blame(mc,n)
      return
    }
    
    //now we must ask for help
    toInvestigate ::= (mc,n,trees)
    
  }

  var iProc = List[(CONode,String)]()
  
  def investigate() : Unit = {

    if((!investigating) || toInvestigate.length == 0) {
      return
    }

    val (mc,n,trees) = toInvestigate(0)
    toInvestigate = toInvestigate.drop(1)

    //gold or blackstack might have updated since preinvestigation
    var nowgold = false
    val trees2 = trees.filter({
      case (tt,ss) => {
        if(allGold(tt))
          nowgold = true
        canGenerate(tt)
      }
    })
    if(nowgold) { //one of the examples vouches for this mc->o
      addkids(mc,n)
      investigate()
      return
    }
    
    println()
    println("INVESTIGATING")
    println(mc)
    println(n.o)
    println()
    
    val MAX_Q = 10
    iProc = trees2.slice(0,MAX_Q).toList

    getSentence = () => {
      val ss = iProc(0)._2
      val tt = iProc(0)._1
      iProc = iProc.drop(1)
      react = (g: Boolean) => {
        if(g) {
          println("Pass...")
          commitAll(tt)
          addkids(mc,n)
          investigate()
        } else {
          blackstack += ss -> tt
        }
        if(iProc.length == 0) {
          println("No more options!")
          blame(mc,n)
        }
      }
      ss
    }
  }

  def newSentence() : String = {
    if(blackstack.size > 0) {
      val cur = blackstack.iterator.next
      blackstack -= cur._1
      if(canGenerate(cur._2)) {
        println("drawing \"" + cur._1 + "\" from blackstack")
        startInvestigation(cur._2) //set up investigation 
      } 
      null
    } else {
      val t = generate()
      if(allGold(t)) {
        //doesn't work, maybe increment a count
        null
      } else {
        react = reactNew(t)
        sdep.realize(t)
      }
    } 
  }

  
  var react : (Boolean) => Unit = (g : Boolean) => {throw new Exception("Un-init")} 
  def reactY() = react(true)
  def reactN() = react(false)
  
  var getSentence : () => String = newSentence

  def reactNew(t : CONode) : (Boolean) => Unit = {
    (g : Boolean) => {
      if(g) {
        commitAll(t)
      } else {
        startInvestigation(t)
      }
    }
  }

  def nextSentence() : String = {
    var s : String = null
    while(s == null) {
      s = getSentence()
    }
    s
  }


  def save(goldF : String, blackF : String) = {
    Active.saveMCO(goldF,goldMCO)
    Active.saveMCO(blackF,blackMCO)
  }
  
}





object Active {

  def command() : String = {
    val valid = List("y","n","q")
    while(true) {
      print("Is this sentence correct? [y/n] :")
      val rl = readLine().trim
      if(valid contains rl)
        return rl
    }
    return ""
  }

  //val tz = DepNode.read("/home/chonger/data/generate/simplewiki/newwiki3.ptb")
  //sdep.addObservations(tz)

  def mainz(args : Array[String]) : Unit = {

    val base = "/home/chonger/data/generate/"
    val sdepF = base + "wiki10.txt"
    val goldF = base + "goldMCO1.txt"
    val blackF = base + "blackMCO1.txt"
    val active = new Active(sdepF,goldF,blackF)

    while(true) {
      println(active.nextSentence())
      
      command() match {
        case "y" => {
          active.react(true)
        }
        case "n" => {
          active.react(false)
        }
        case "q" => {
          active.save(goldF,blackF)
          sys.exit(0)
        }
      }

    }

  }


  /**

    var gg = 0
    while(true) {
      if(active.blackstack.size > 0) {
        val cur = active.blackstack.iterator.next
        active.blackstack -= cur._1
        if(active.canGenerate(cur._2)) {
          println("drawing \"" + cur._1 + "\" from blackstack")
          active.startInvestigation(cur._2)
        }
      } else {
        val t = active.generate()
        if(active.allGold(t)) {
          gg += 1
          if(gg == 100000) {
            println("Generated " + gg + " pure gold in a row - terminating")
            0.until(100).foreach(i => {
              println(active.sdep.realize(active.generate()))
            })
            saveMCO(goldF,active.goldMCO)
            saveMCO(blackF,active.blackMCO)
            sys.exit(0)
          } 
        } else {
          if(gg > 0) 
            println("Just generated " + gg + " pure gold in a row!")
          gg = 0
          println(active.sdep.realize(t))
          active.command() match {
            case "y" => {
              active.commitAll(t)
            }
            case "n" => {
              active.startInvestigation(t)
            }
            case "q" => {
              saveMCO(goldF,active.goldMCO)
              saveMCO(blackF,active.blackMCO)
              sys.exit(0)
            }
          }
        }
      }
    }

  }
*/     
  def saveMCO(file : String, mco : HashMap[MaxContext,HashSet[AllOutcome]]) = {
    import java.io._
    val bw = new BufferedWriter(new FileWriter(file))
    mco.foreach({
      case (mc,os) => {
        bw.write(mc.save() + "\n")
        bw.write(os.size + "\n")
        os.foreach(o => {
          bw.write(o.save() + "\n")
        })
      }
    })
    
    bw.close()
  }

  def loadMCO(file : String) : HashMap[MaxContext,HashSet[AllOutcome]] = {
    val mco = new HashMap[MaxContext,HashSet[AllOutcome]]()
    
    val ff = io.Source.fromFile(file)
    val lines = ff.getLines()

    while(lines.hasNext) {
      val mc = MaxContext.load(lines.next.trim)
      val nO = lines.next.trim.toInt
      val hs = new HashSet[AllOutcome]()
      0.until(nO).foreach(ii => {
        hs += AllOutcome.load(lines.next.trim)
      })
      mco += mc -> hs
    }
    
    ff.close()
    mco
  }

}
