import multitool._
import scala.collection.mutable.{HashMap,HashSet}

abstract class Lookup {
  def inLexicon(l : Lexicon) : Boolean
  def hasWord(w : String) : Boolean = false
}


abstract class DepGramBase[Context <: Lookup,Outcome <: Lookup] {

  def getObservations(t : DepNode) : List[(Context,Outcome)]
  def implies(c : Context, o : Outcome) : List[Context] 
  def smoothImplies(a : Any, o : Outcome) : List[Any] = Nil
  def rootContext : Context 

  val genProbs = new HashMap[Context,HashMap[Outcome,Double]]()

  val smoothedOuts = new HashMap[Any,HashMap[Outcome,Double]]()
  
  val backSOuts = new HashMap[Any,HashMap[Any,Double]]()
  val backOuts = new HashMap[Any,HashMap[Outcome,Double]]()

  val trueSOuts = new HashMap[Context,HashMap[Any,Double]]()
  val trueOuts = new HashMap[Context,HashMap[Outcome,Double]]()

  val trueTots = new HashMap[Context,Double]()
  val smoothTots = new HashMap[Any,Double]()


  //default is no smoothing
  def smoothC(c : Context) : Any = c 
  def smoothO(o : Outcome) : Any = o

  val alpha = 10.0

  def choose(c : Context) : Outcome = {
    Babble.sample(genProbs(c).iterator)
  }

  var full_OC = new HashMap[Context,HashMap[Outcome,Double]]()
  var full_sOC = new HashMap[Context,HashMap[Any,Double]]()
  var full_OsC = new HashMap[Any,HashMap[Outcome,Double]]()
  var full_sOsC = new HashMap[Any,HashMap[Any,Double]]()
  var full_OsO = new HashMap[Any,HashMap[Outcome,Double]]()
  val lambda = new HashMap[Context,Double]()
  val gammaC = new HashMap[Context,Double]()
  val gammaSC = new HashMap[Any,Double]()
  def backoffFromC(c : Context) = lambda.getOrElse(c,0.0)
  def backoffOfromC(c : Context) = gammaC.getOrElse(c,0.0)
  def backoffOfromSC(sC : Any) = gammaSC.getOrElse(sC,0.0)
  

/**
  def backoffFromC(c : Context) = {
    if((trueOuts contains c) || (trueSOuts contains c)) {
      .5
    } else {
      0.0
    }
  }

  def backoffOfromSC(sC : Any) = {
    if(backOuts contains sC) {
      if(backSOuts contains sC) {
        .5
      } else {
        1.0
      }
    } else {
      0.0
    }
  }

  def backoffOfromC(c : Context) = {
    if(trueOuts contains c) {
      if(trueSOuts contains c) {
        .5
      } else {
        1.0
      }
    } else {
      0.0
    }
  } 
*/

  /**
  def chooseSmooth(c : Context) : Outcome = {
  val backoffC = Babble.rando.nextDouble() >= backoffFromC(c)
    if(backoffC) {
      val sC = smoothC(c)
      val backoffO = Babble.rando.nextDouble() >= backoffOfromSC(c)
      if(backoffO) {
        var sO : Any = null
        try {
          sO = Babble.sample(backSOuts(sC).iterator)
          Babble.sample(smoothedOuts(sO).iterator)
        } catch {
          case e : Exception => {
            backSOuts(sC).iterator.foreach(x => println(x))
            if(sO != null) {
              println("!!")
              smoothedOuts(sO).iterator.foreach(x => println(x))
            }
            throw e
          }
        }
      } else {
        Babble.sample(backOuts(sC).iterator)
      }
    } else {
      val backoffO = Babble.rando.nextDouble() >= backoffOfromC(c)
      if(backoffO) {
        val sO = Babble.sample(trueSOuts(c).iterator)
        Babble.sample(smoothedOuts(sO).iterator)
      } else {
        Babble.sample(trueOuts(c).iterator)
      }
    }
  }
  */

  val probs = new HashMap[Context,HashMap[Outcome,Double]]()
  val cprobs = new HashMap[Context,Double]()

  def getContextProbs(maxDepth : Int) = {
    var cur = new HashMap[Context,Double]()
    cur += rootContext -> 1.0

    0.until(maxDepth).foreach(i => {
      var next = new HashMap[Context,Double]()
      var totEx = 0.0
      cur.iterator.foreach({
        case (c,count) => {
          probs(c).foreach({
            case (o,d) => {
              implies(c,o).foreach(cc => {
                addmap(cc,d*count,next)
              })
            }
          })
          totEx += count
          addmap(c,count,cprobs)
        }
      })
      cur = next
      println("TOTAL Expected context counts - " + totEx)
    })
    cur.iterator.foreach({case (c,count) => addmap(c,count,cprobs)})
    normalize(cprobs)
  }


  def getRealProbs(context : Context) : HashMap[Outcome,Double] = {
    val lam = lambda.getOrElse(context,0.0) //we might as for a unseen true context
    val sC = smoothC(context)
    val outs = new HashMap[Outcome,Double]()
    if(lam > 0) { //theres some real prob
//      println("!")
  //    println(genProbs contains context)
    //  println(full_sOC contains context)
      //println(full_OC contains context)
      val gamC = gammaC(context)
      full_OC(context).iterator.foreach(o => addmap(o._1,lam*gamC*o._2,outs))
      full_sOC(context).iterator.foreach(sO => {
        val p1 = sO._2
        full_OsO(sO._1).iterator.foreach(o => {
          val v = lam*(1.0-gamC)*p1*o._2
          addmap(o._1,v,outs)
        })
      })
    }
    
    val gamSC = gammaSC(sC)    //the smooth Context MUST be there
    full_OsC(sC).iterator.foreach(o => addmap(o._1,(1.0 - lam)*gamSC*o._2,outs))
    full_sOsC(sC).iterator.foreach(sO => {
      val p1 = sO._2
      full_OsO(sO._1).iterator.foreach(o => {
        val v = (1.0-lam)*(1.0-gamSC)*p1*o._2
        addmap(o._1,v,outs)
      })
    })
    outs
  }
  
  def setProbs() = {
    println("SET PROBS!!!!!")
    val allContexts = getAllContexts() //this is all consistent contexts

    //assumes every C -> smoothOutcome (C,0) is in realProbs
    //this is true - smoothOutcomes are the consistent subset of connections
    //realProbs is the full
    
    val edges = new HashMap[Context,HashMap[Context,Double]]()

    allContexts.foreach(c => {
      val outs = outcomesSmooth(c)
      val oprobs = getRealProbs(c)
      outs.iterator.foreach(o => {
        val d = oprobs(o)
        implies(c,o).foreach(cc => {
          assert(allContexts contains cc)
          addmap(cc,d,edges.getOrElseUpdate(c,new HashMap[Context,Double]()))
        })
      })
    }) 

    import scala.collection.mutable.ArrayBuffer
    //do topo sort
    var tEdges = new ArrayBuffer[(Context,Context,Double)]()

    tEdges ++= edges.iterator.flatMap(x => x._2.map(y => (x._1,y._1,y._2))).toArray.sortWith(_._3 > _._3)

    /**
    println("Drop cycles - " + tEdges.length + " edges to check")
    
    def checkEdge(s : Context, t : Context) : Boolean = {
      var proc = List[Context](t)
      while(proc.length > 0) {
        var cur = proc(0)
        if(cur == s) 
          return false
        proc = kEdges.getOrElse(cur,new HashMap[Context,Double]()).keysIterator.toList ::: proc.drop(1)
      } 
      true
    }
    tEdges.foreach(e => {
      if(checkEdge(e._1,e._2))
        addmap(e._2,e._3,kEdges.getOrElseUpdate(e._1,new HashMap[Context,Double]()))
    })
    */

    def bSearch(st : Int, end : Int) : List[Context] = {
      var half = (st + end) / 2
      if(st + 1 == end)
        half += 1
      //println("BSEARCH - " + st + " - " + end + " - " + half)
      
      val tryTopo = topoSort(tEdges.slice(0,half).toArray)
      if(st == end) {
        println("using " + end + " out of " + tEdges.length)
        if(end < tEdges.length) {
          tEdges.remove(end)
          //println("RESTART")
          return bSearch(st,tEdges.length)
        } else {
          return tryTopo
        }
      }
      if(tryTopo == null) {
        if(st + 1 == end)
          half -= 1
        bSearch(st,half)
      } else {
        bSearch(half,end)
      }
    } 
    
    def topoSort(edg : Array[(Context,Context,Double)]) : List[Context] = {
      //println("Topo sort")

      val kEdges = new HashMap[Context,HashMap[Context,Double]]()
      edg.foreach(e => {
          addmap(e._2,e._3,kEdges.getOrElseUpdate(e._1,new HashMap[Context,Double]()))
      })

      val inDeg = new HashMap[Context,Int]()
      kEdges.iterator.foreach(_._2.iterator.foreach(x => {
        val e = inDeg.getOrElse(x._1,0) + 1
        inDeg += x._1 -> e
      }))
      
      var proc = List[Context]()
      //if its not in inDeg, its got in degree of 0
      allContexts.foreach(c => {
        if(!(inDeg contains c))
          proc ::= c
      })

      //println("START NODES - " + proc.length)
      //assert(proc contains rootContext)

      var topoR = List[Context]()
      while(proc.length > 0) {
        var cur = proc(0)
        proc = proc.drop(1)
        topoR ::= cur
        kEdges.getOrElse(cur,new HashMap[Context,Double]()).iterator.foreach(x => {
          var e = inDeg(x._1)
          e -= 1
          inDeg += x._1 -> e
          if(e == 0) {
            proc ::= x._1
          } 
        })
      }

      if(topoR.length < allContexts.size)
        null
      else
        topoR
    }      
    println(allContexts.size)
    println(tEdges.length)
    val topo = bSearch(0,tEdges.length)

    println("Calc Goodness - " + topo.length)

    val goodness = new HashMap[Context,Double]()
    topo.foreach(c => {
      var g = 0.0
      val rp = getRealProbs(c)
      outcomesSmooth(c).foreach(o => {
        g += (rp(o) /: implies(c,o))((a,b) => a * goodness.getOrElse(b,0.0))
      })
      goodness += c -> g
    })

    println("root goodness - " + goodness(rootContext))

    allContexts.foreach(c => {
      val oprobs = getRealProbs(c)
      val outs = outcomesSmooth(c)
      val ps = outs.iterator.map(o => {
        val v = (oprobs(o) /: implies(c,o))((a,b) => {
          a * goodness(b)
        })
        (o,v)
      }).toList

      val tot = (0.0 /: ps)(_ + _._2)
      val os = new HashMap[Outcome,Double]() 
      ps.foreach(x => {
        val v = x._2/tot
        os += x._1 -> v
      })
      probs += c -> os
    })

    probs.foreach(x => normalize(x._2))
    getContextProbs(15)
    println("DONE SET PROBS!!!!!")
  }

  def chooseSmooth(c : Context) : Outcome = {
    try {
      Babble.sample(probs(c).iterator)
    } catch {
      case e : Exception => {
        println(c)
        probs(c).foreach(x => println(x))
        throw e
      }
    }
  }

  def outcomesSmooth(c : Context) : List[Outcome] = {
    val outs = new HashSet[Outcome]()
    if(trueOuts contains c) {
      outs ++= trueOuts(c).keysIterator
    }
    if(trueSOuts contains c) {
      trueSOuts(c).keysIterator.foreach(o => {
        outs ++= smoothedOuts(o).keysIterator
      })
    }
    val sC = smoothC(c)
    if(backOuts contains sC) {
      outs ++= backOuts(sC).keysIterator
    }
    if(backSOuts contains sC) {
      backSOuts(sC).keysIterator.foreach(o => {
        outs ++= smoothedOuts(o).keysIterator
      })
    }
    outs.toList
  }

  def debugPaths(c : Context) = {
    var outs = new HashSet[Outcome]()
    if(trueOuts contains c) {
      outs ++= trueOuts(c).keysIterator
      outs.iterator.foreach(o => {println("1" + o)})
    }
    outs = new HashSet[Outcome]()
    if(trueSOuts contains c) {
      trueSOuts(c).keysIterator.foreach(o => {
        outs ++= smoothedOuts(o).keysIterator
      })
      outs.iterator.foreach(o => {println("2" + o)})
    }
    outs = new HashSet[Outcome]()
    val sC = smoothC(c)
    if(backOuts contains sC) {
      outs ++= backOuts(sC).keysIterator
      outs.iterator.foreach(o => {println("3" + o)})
    }
    outs = new HashSet[Outcome]()
    if(backSOuts contains sC) {
      backSOuts(sC).keysIterator.foreach(o => {
        outs ++= smoothedOuts(o).keysIterator
      })
      outs.iterator.foreach(o => {println("4" + o)})
    }
/**
    val lam = lambda.getOrElse(context,0.0) //we might as for a unseen true context
    val sC = smoothC(context)
    val outs = new HashMap[Outcome,Double]()
    if(lam > 0) { //theres some real prob
      val gamC = gammaC(context)
      full_OC(context).iterator.foreach(o => println("!1" + o._1))
      full_sOC(context).iterator.foreach(sO => {
        full_OsO(sO._1).iterator.foreach(o => {
          lam*(1.0-gamC)*p1*o._2
        })
      })
    }
    
    val gamSC = gammaSC(sC)    //the smooth Context MUST be there
    full_OsC(sC).iterator.foreach(o => addmap(o._1,(1.0 - lam)*gamSC*o._2,outs))
    full_sOsC(sC).iterator.foreach(sO => {
      val p1 = sO._2
      full_OsO(sO._1).iterator.foreach(o => {
        (1.0-lam)*(1.0-gamSC)*p1*o._2
      })
    })
*/
  }

  def chooseBasic(c : Context) : Outcome = {
    Babble.sample(getRealProbs(c))
  }

  def generate() : DepNode = compose(rootContext,choose)
  def generateSmooth() : DepNode = compose(rootContext,chooseSmooth)
  def compose(c : Context, f : (Context) => Outcome) : DepNode
  def composeChain(chain : List[(Context,Outcome)], f : (Context) => Outcome) : DepNode = null
  /**
   def sampleTables(a : Double, c : Double) = {
   var t = 0
   0.until(c.toInt).foreach(i => {
   if(Babble.rando.nextDouble() < (alpha / (alpha + t)))
   t += 1
   })
   t
   }  
   */

  def trainSmooth(trees : Array[DepNode], lexicon : Lexicon) = {
    firstPass(lexicon) // use the in vocab observations to define a set of smoothed context/outcomes
    //secondPass() // add in all counts restricted to "good" smoothed c/o's

    normSmooth()
  }

  def addmap[A](k : A, dd : Double, m : HashMap[A,Double]) : Unit = {
    var e : Double = m.getOrElse(k, 0.0)
    e += dd
    m += k -> e
  }

  def limitCounts(n : Int) = {
    println("LIMITING - starting with " + (0 /: genProbs.iterator)(_ + _._2.size))
    genProbs.iterator.foreach({
      case (context,outs) => {
        outs.iterator.foreach({
          case (o,c) => {
            if(c < n)
              outs -= o
          }
        })
        if(outs.size == 0)
          genProbs -= context
      }
    })
    println("LIMITING - ending with " + (0 /: genProbs.iterator)(_ + _._2.size))
  }  



  def firstPass(lexicon : Lexicon) = {
    
    //in the first pass we detemine the C', O' and O sets that we 
    
    println("first pass " + genProbs.size)
    
    var sContextSet = new HashSet[Any]()
    var sOutcomeSet = new HashSet[Any]()
    var outcomeSet = new HashSet[Outcome]()

    genProbs.iterator.foreach({
      case (context,outs) => {
        sContextSet += smoothC(context)
        outs.iterator.foreach({
          case (outcome,count) => {
            if(outcome.inLexicon(lexicon)) {
              //dont bother adding a smoothO that won't connect to an in-vocab outcome
              sOutcomeSet += smoothO(outcome)
              outcomeSet += outcome
            }
          }
        })
      }
    })
    
    println(sContextSet.size + " smooth contexts")
    println(sOutcomeSet.size + " smooth outcomes")
    println(outcomeSet.size + " true outcomes")
    
    smoothedOuts.clear()
    
    backSOuts.clear()
    backOuts.clear()
    
    trueSOuts.clear()
    trueOuts.clear()
    
    val backCmap = new HashMap[Any,List[Context]]()
    
    //get the superset of all connections, which will be pruned down

    genProbs.iterator.foreach({
      case (context,outs) => {
        //input - C -> Map[O,count]
     
        val cSmooth = smoothC(context)
        
        //Paths TT/TS
        if(context.inLexicon(lexicon)) {
          //first get C -> O' counts (TS)
          val newM = new HashMap[Any,Double]()
          outs.iterator.foreach(x => {
            val sO = smoothO(x._1) 
            if(sOutcomeSet contains sO)
              newM += sO -> x._2 
          })
          trueSOuts += context -> newM

          //then C -> O counts
          val goodouts = outs.filter(outcomeSet contains _._1)
          if(goodouts.size > 0)
            trueOuts += context -> goodouts
          
          //remember which explicit contexts are connected to each smoothC 
          val e = context :: backCmap.getOrElseUpdate(cSmooth,List[Context]())
          backCmap += cSmooth -> e
        }
        
        //paths ST/SS
        if(sContextSet contains cSmooth) {
          
          val sOuts = backSOuts.getOrElseUpdate(cSmooth,new HashMap[Any,Double]())
          val tOuts = backOuts.getOrElseUpdate(cSmooth,new HashMap[Outcome,Double]())
          
          outs.iterator.foreach({
            case (outcome,count) => {
              //ST
              if(outcomeSet contains outcome) {
                addmap(outcome,count,tOuts)
              }

              //SS
              val outcomeS = smoothO(outcome)
              if(sOutcomeSet contains outcomeS) {
                addmap(outcomeS,count,sOuts)
                if(outcomeSet contains outcome) {
                  val om = smoothedOuts.getOrElseUpdate(outcomeS,new HashMap[Outcome,Double])
                  addmap(outcome,count,om)
                }
              }
            }
          })
        }
      }
    })  

    println("counts are counted, starting to prune")

    //now prune stuff out - we must never make a C->O that implies an out of model C_

    def chkS(ccc : Any) = sContextSet contains ccc
    def chkC(ccc : Context) = chkS(smoothC(ccc))

    def pruneOut() = {

      var diff = 0

      //TT path
      trueOuts.iterator.foreach({
        case (ctx,os) => {
          os.iterator.foreach({
            case (o,c) => {
              if(!implies(ctx,o).forall(chkC(_)))
                os -= o
            }
          })
          if(os.size == 0)
            trueOuts -= ctx
        }
      })

      //ST path
      backOuts.iterator.foreach({
        case (ctx,os) => {
          os.iterator.foreach({
            case (o,c) => {
              if(!smoothImplies(ctx,o).forall(chkS(_)))
                os -= o
            }
          })
          if(os.size == 0)
            backOuts -= ctx
        }
      })
      
      //SS path
      //assumes that we only need to check C_ and use backmap
      //I(C_,O) means that if a smooth C fails then all its true C's will too
      backSOuts.iterator.foreach({
        case (ctx,os) => {
          os.iterator.foreach({
            case (o,c) => {
              val ok = smoothedOuts(o).forall(x => {
                smoothImplies(ctx,x._1).forall(chkS(_))
              })
              if(!ok) {
                os -= o
              }
            }
          })
          if(os.size == 0)
            backSOuts -= ctx
          
        }
      })

      //TS path
      trueSOuts.iterator.foreach({
        case (ctx,os) => {
          os.iterator.foreach({
            case (o,c) => {
              val ok = smoothedOuts(o).forall(x => {
                implies(ctx,x._1).forall(chkC(_))
              })
              if(!ok) {
                os -= o
              }
            }
          })
          if(os.size == 0)
            trueSOuts -= ctx
        }
      })

      //now clean up the mess- 
      //O_->O is always untouched, only problem is that some O_ may be unreachable now

      sContextSet.iterator.toList.foreach(cc => {
        if(!((backOuts contains cc) || (backSOuts contains cc))) {
          sContextSet -= cc
          diff += 1
        }
      })

      diff
    }

    //println("SMOOTH C's - " + sContextSet.size)
    //sContextSet.foreach(k => println(k))

    var dropped = 100
    while(dropped > 0) {
      dropped = pruneOut()
      println("Smooth Contexts dropped = " + dropped + " (" + sContextSet.size + " left)")
    }


    println("everything in pruned, now remove unreachable stuff")

    //check the assertion and kill unreachable contexts
    val allContexts = getAllContexts()
    println("All Contexts : " + allContexts.size)
    val finalSC = new HashSet[Any]() ++ allContexts.map(x => smoothC(x))
    println("Reachable S-Contexts : " + finalSC.size)

    var finalSO = new HashSet[Any]()
    allContexts.foreach(c => {
      if(trueSOuts contains c) {
        trueSOuts(c).iterator.foreach(x => finalSO += x._1)
      }
    })
    finalSC.foreach(cc => {
      if(backSOuts contains cc) {
        backSOuts(cc).iterator.foreach(x => finalSO += x._1)
      }
    })
    println("Reachable S-Outcomes : " + finalSO.size + " (" + smoothedOuts.size + ")")

    val finalO = new HashSet[Outcome]()
    finalSO.foreach(x => {
      finalO ++= smoothedOuts(x).iterator.map(_._1)
    })
    println("Reachable Outcomes : " + finalO.size + " (" + outcomeSet.size + ")")
    
    removeUnreachable(finalSO,smoothedOuts)
    removeUnreachable(allContexts,trueSOuts)
    removeUnreachable(allContexts,trueOuts)
    removeUnreachable(finalSC,backOuts)
    removeUnreachable(finalSC,backSOuts)

  }

  def getAllContexts() = {

    var allContexts = new HashSet[Context]() + rootContext
    println("Observed Contexts : " + allContexts.size)
    var possible = new HashSet[Context]() ++ allContexts
    
    var newC = 100 
    while(newC > 0) {
      newC = 0
      var check = possible.iterator.toList
      possible.clear()
      
      check.foreach(c => {

        //this checks the SC assertion
        if(!((backSOuts contains smoothC(c)) || (backOuts contains smoothC(c)))) {
          println("my smoothC = " + smoothC(c))
          println(c.asInstanceOf[NGramContext].prev)
          println("BAD CONTEXT - " + c)
          assert(false)
        }

        val os = outcomesSmooth(c)
        os.flatMap(o => implies(c,o)).foreach(x => {
          if(allContexts contains x) {
            //all is well
          } else {
            newC += 1
            possible += x
            allContexts += x
          }
        })
      })
      
      println("NEW: " + newC + " - Tot = " + allContexts.size)

    }
    allContexts
  }

  def removeUnreachable[A,B](good : HashSet[A], m : HashMap[A,B]) {
    m.iterator.foreach({
      case (o,c) => {
        if(! (good contains o))
          m -= o
      }
    })
  }
    
  def zeroOut[A,B](m : HashMap[A,HashMap[B,Double]]) = {
    m.foreach({
      case (k,mm) => {
        mm.iterator.foreach(mm += _._1 -> 0.0)
      }
    })    
  }

  def em() = {

    //init branch probs 
    //make dists for each step on full data
    genProbs.iterator.foreach({
      case (context,outs) => {
        val sC = smoothC(context)
        lambda += context -> (.25 + Babble.rando.nextDouble() / 2.0)
        gammaC += context -> (.25 + Babble.rando.nextDouble() / 2.0)
        gammaSC += sC -> (.25 + Babble.rando.nextDouble() / 2.0)
        outs.iterator.foreach({
          case (o,count) => {
            val sO = smoothO(o)
            addmap(o,count,full_OC.getOrElseUpdate(context,new HashMap[Outcome,Double]()))
            addmap(sO,count,full_sOC.getOrElseUpdate(context,new HashMap[Any,Double]()))
            addmap(o,count,full_OsC.getOrElseUpdate(sC,new HashMap[Outcome,Double]()))
            addmap(sO,count,full_sOsC.getOrElseUpdate(sC,new HashMap[Any,Double]()))
            addmap(o,count,full_OsO.getOrElseUpdate(sO,new HashMap[Outcome,Double]()))
          }
        })
      }
    })    

    def normEstimate() = {
      full_OC.iterator.foreach(x => normalize(x._2))
      full_sOC.iterator.foreach(x => normalize(x._2))
      full_OsC.iterator.foreach(x => normalize(x._2))
      full_sOsC.iterator.foreach(x => normalize(x._2))
      full_OsO.iterator.foreach(x => normalize(x._2))
    }

    normEstimate()

    def iter() = {
      
      var ll = 0.0

      //E STEP

      val ex_OC = full_OC.map(x => (x._1,x._2.map(y => (y._1,1.0))))
      val ex_sOC = full_sOC.map(x => (x._1,x._2.map(y => (y._1,1.0))))
      val ex_OsC = full_OsC.map(x => (x._1,x._2.map(y => (y._1,1.0))))
      val ex_sOsC = full_sOsC.map(x => (x._1,x._2.map(y => (y._1,1.0))))
      val ex_OsO = full_OsO.map(x => (x._1,x._2.map(y => (y._1,1.0))))
      
      val ex_lambda = lambda.map(x => (x._1,Array(1.0,1.0)))
      val ex_gammaC = gammaC.map(x => (x._1,Array(1.0,1.0)))
      val ex_gammaSC = gammaSC.map(x => (x._1,Array(1.0,1.0)))

      genProbs.iterator.foreach({
        case (context,outs) => {
          val lam = lambda(context)
          val sC = smoothC(context)
          outs.iterator.foreach({
            case (o,count) => {
              val sO = smoothO(o)
              var pathProbs = new HashMap[String,Double]()
              val gamC = gammaC(context)
              val gamSC = gammaSC(sC)
              
              pathProbs += "TT" -> lam*gamC*full_OC(context)(o)
              pathProbs += "TS" -> lam*(1.0-gamC)*full_sOC(context)(sO)*full_OsO(sO)(o)
              pathProbs += "ST" -> (1.0-lam)*gamC*full_OsC(sC)(o)
              pathProbs += "SS" -> (1.0-lam)*(1.0-gamC)*full_sOsC(sC)(sO)*full_OsO(sO)(o)

              var totM = (0.0 /: pathProbs)(_ + _._2)
              ll += count * Math.log(totM)
              totM = count / totM

              pathProbs = pathProbs.map(x => (x._1,x._2*totM))

              addmap(o,pathProbs("TT"),ex_OC(context))
              addmap(sO,pathProbs("TS"),ex_sOC(context))
              addmap(o,pathProbs("ST"),ex_OsC(sC))
              addmap(sO,pathProbs("SS"),ex_sOsC(sC))
              addmap(o,pathProbs("SS") + pathProbs("TS"),ex_OsO(sO))

              ex_lambda(context)(0) += pathProbs("TT") + pathProbs("TS")
              ex_lambda(context)(1) += pathProbs("ST") + pathProbs("SS")
              ex_gammaC(context)(0) += pathProbs("TT")
              ex_gammaC(context)(1) += pathProbs("TS")
              ex_gammaSC(sC)(0) += pathProbs("ST")
              ex_gammaSC(sC)(1) += pathProbs("SS")

            }
          })
        }
      })

      println("LOG LIKE: " + ll)

      //MSTEP

      full_OC = ex_OC
      full_sOC = ex_sOC
      full_OsC = ex_OsC
      full_sOsC = ex_sOsC
      full_OsO = ex_OsO
      normEstimate()

      ex_lambda.foreach({ case (c,a) => lambda(c) = a(0) / (a(0) + a(1)) })
      ex_gammaC.foreach({ case (c,a) => gammaC(c) = a(0) / (a(0) + a(1)) })
      ex_gammaSC.foreach({ case (c,a) => gammaSC(c) = a(0) / (a(0) + a(1)) })

    }

    0.until(20).foreach(i => {
      println(i)
      iter()
    })

  }


  def normSmooth() = {
     //normalize all
    smoothedOuts.iterator.foreach(x => normalize(x._2))
    backOuts.iterator.foreach(x => normalize(x._2))
    backSOuts.iterator.foreach(x => normalize(x._2))
    trueSOuts.iterator.foreach(x => normalize(x._2))
    trueOuts.iterator.foreach(x => normalize(x._2))

  }
  
  def train(trees : Array[DepNode]) = {
    genProbs.clear()
    addObservations(trees)
    normalize()
  }

  def addObservations(trees : Array[DepNode]) = {

    trees.foreach(t => {
      getObservations(t).foreach({
        case (context,outcome) => {
          val gMap = genProbs.getOrElseUpdate(context,new HashMap[Outcome,Double]())
          val gE = gMap.getOrElse(outcome,0.0) + 1.0
          gMap += outcome -> gE
        }
      })      
    })

  }

  def normalize[X](m : HashMap[X,Double]) : Unit = {
    val tot = (0.0 /: m.iterator)(_ + _._2)
    if(tot == 0) {
      println("WARNING")
      m.iterator.foreach(x => {
        println(x._1 + " -----> " + x._2)
      })
    }
    m.iterator.foreach({
      case (kk,vv) => m += kk -> vv/tot
    })
  }
  
  def normalize() : Unit = {
    genProbs.iterator.foreach({
      case (k,m) => {
        normalize(m)
      }
    })
  }

  def unique() = {
    (0 /: genProbs.iterator)(_ + _._2.size)
  }

  def limit(lexicon : Lexicon, discount : Int) = {

    println("starting with " + unique())

    var good = List[(Context,Outcome,Double)]()

    genProbs.iterator.foreach({
      case (con,m) => {
        if(con.inLexicon(lexicon)) {
          m.iterator.foreach({
            case (out,d) => {
              if(d > discount && out.inLexicon(lexicon)) {
                good ::= (con,out,d)
              }
            }
          })
        }
      }
    })

    genProbs.clear()
    good.foreach({
      case (gk,out,d) => {
        val m = genProbs.getOrElseUpdate(gk,new HashMap[Outcome,Double]())
        m += out -> (d - discount)
      }
    })

    println("Now it's " + good.length)    


  }

  def prune() = {
    var changed = 100
    while(changed > 0) {
      changed = 0
      var empty = List[Context]()
      genProbs.foreach({
        case (gK,m) => {
          m.iterator.foreach({
            case (out,d) => {
              val ok = (true /: implies(gK,out))((a,b) => a && (genProbs contains b))
              if(!ok) {
                changed += 1
                m -= out
                if(m.size == 0) {
                  empty ::= gK
                }
              }
            }
          })
        }
      })
      

      empty.foreach(e => genProbs -= e)
      println("Removed " + changed + " generations")
    }

    println("at the end we have " + unique())

    genProbs.foreach({
      case (gK,m) => {
        m.iterator.foreach({
          case (out,d) => {
            implies(gK,out).foreach(x => assert(genProbs contains x))
          }
        })
      }
    })

    var found = new HashSet[Context]()

    def find(c : Context) : Unit = {
      if(!(found contains c)) {
        found += c
        genProbs(c).iterator.map(_._1).foreach(o => {
          implies(c,o).foreach(cc => find(cc))
        })
      }
    }
    find(rootContext)

    val notFound = genProbs.iterator.map(_._1).filter(x => !(found contains x))
    notFound.foreach(x => genProbs -= x)
    
    println("after removing dead contexts we have " + unique() + " C,O pairs")

    genProbs.foreach({
      case (gK,m) => {
        m.iterator.foreach({
          case (out,d) => {
            implies(gK,out).foreach(x => assert(genProbs contains x))
          }
        })
      }
    })


  }


/**
  def outcomesSmoothWithP(c : Context) : List[(Outcome,Double)] = {
    val outs = new HashMap[Outcome,Double]()
    if(trueOuts contains c) {
      trueOuts(c).keyIterator
    }
    if(trueSOuts contains c) {
      trueSOuts(c).keyIterator.foreach(o => {
        outs ++= smoothedOuts(o).keyIterator
      })
    }
    val sC = smoothC(c)
    if(backOuts contains sC) {
      outs ++= backOuts(c).keyIterator
    }
    if(backSOuts contains sC) {
      backSOuts(sC).keyIterator.foreach(o => {
        outs ++= smoothedOuts(o).keyIterator
      })
    }
    outs.iterator.toList
  }
*/
  /**

  def smoothInfo() = {
    println(smoothedOuts.size + " smoothed outcomes")
    println(backProbs.size + " smoothed contexts")
    println(frontProbs.size + " actual contexts")
  }


  def outcomesSmooth(c : Context) = {
    val sC = smoothC(c)
    val backOs = backProbs(sC)
    val allSOs = frontProbs.getOrElse(c,new HashMap[Any,Double]()).iterator.map(_._1).toList ::: backOs.iterator.map(_._1).toList
    (new HashSet[Outcome]() ++ allSOs.flatMap(o => smoothedOuts(o).iterator.map(_._1))).toList
  }

  def limitSmooth(lexicon : Lexicon) = {
    
    println("start limit")

    var goodC =  HashMap[Context,Boolean]()
    var goodCO =  HashMap[(Context,Outcome),Boolean]()
    
    def checkC(c : Context) : Boolean = {
      if(goodC contains c) 
        return goodC(c)

      val g = if(inLexiconC(c,lexicon)) {
        val outs = outcomesSmooth(c)
        var ok = false
        outs.foreach(o => {
          if(checkCO(c,o))
            ok = true
        })
        ok
      } else {
        false
      }

      goodC += c -> g
      g
    }

    def checkCO(c : Context, o : Outcome) : Boolean = {
      val k = (c,o)
      if(goodCO contains k) 
        return goodCO(k)

      val g = if(inLexiconO(o,lexicon)) {
        val im = implies(c,o)
        var ok = true
        im.foreach(c => {
          if(checkC(c))
            ok = false
        })
        ok
      } else {
        false
      }

      goodCO += k -> g
      g
    }

    val rootGood = checkC(rootContext)
    println("rootgood "  + rootGood)
  }
*/
/**
  def limitSmooth(lexicon : Lexicon) = {
 
    //dont change smoothed estimates at all, just prune frontProbs and smoothedOuts

    //val frontProbs = new HashMap[Context,HashMap[Any,Double]]()

    var bad = List[Context]()
    frontProbs.iterator.foreach({
      case (con,m) => {
        if(!inLexiconC(con,lexicon)) bad ::= con
      }
    })
    bad.foreach(c => frontProbs -= c)

    //val smoothedOuts = new HashMap[Any,HashMap[Outcome,Double]]()

    smoothedOuts.foreach({
      case (a,os) => {
        var obad = List[Outcome]()
        os.iterator.foreach({
          case (o,d) => {
            if(!inLexiconO(o,lexicon))
              obad ::= o
          }
        })
        obad.foreach(o => os -= o)
      }
    })

    def pruneBack() = {

      //get rid of context and smooth outcomes if the dont lead to any outcomes anymore

      var bad = List[Any]()
      smoothedOuts.foreach({
        case (a,os) => {
          if(os.size == 0) bad ::= a
        }
      })
      bad.foreach(a => smoothedOuts -= a)

      var bad2 = List[Context]()
      frontProbs.foreach({
        case (a,os) => {
          os.foreach({
            case (o,d) => {
              if(!(smoothedOuts contains o))
                os -= o
            }
          })
          if(os.size == 0) bad2 ::= a
        }
      })
      bad2.foreach(a => {
        frontProbs -= a
        frontTots -= a
      })

      var bad3 = List[Any]()
      backProbs.foreach({
        case (a,os) => {
          os.foreach({
            case (o,d) => {
              if(!(smoothedOuts contains o))
                os -= o
            }
          })
          if(os.size == 0) bad3 ::= a
        }
      })
      bad3.foreach(a => backProbs -= a)
    }

    def pruneFor() = {
      
      //eliminate context -> outcomes that imply contexts with empty smoothing dists
      

      var badC = List[Context]()

      frontProbs.foreach({
        case (c, os) => {
          val sC = smoothC(c)
          val backOs = backProbs(sC)
          val allSOs = os.iterator.map(_._1).toList ::: backOs.iterator.map(_._1).toList
          val allOs = allSOs.flatMap(o => smoothedOuts(o).iterator.map(_._1))
          
          val badOs = allOs.filter(o => {
            !(true /: implies(c,o))((a,b) => a && (backProbs contains smoothC(b)))
          }).map(x => smoothO(x))

          if(sC == List(6,3,2,25,0)) {
            println("!")
            badOs.foreach(x => println(x))
          }

          badOs.foreach(bad => {
            if(os contains bad) os -= bad
            if(backOs contains bad) backOs -= bad
          })

          if(os.size == 0)
            badC ::= c

        }
      })

      badC.foreach(c => {
        frontProbs -= c
        frontTots -= c
      })

    }

    pruneBack()
    pruneFor()

  }
*/


/**  
  def secondPass() = {

    //now we need to estimate all the probs
    
    zeroOut(smoothedOuts)
    zeroOut(backOuts)
    zeroOut(backSOuts)
    zeroOut(trueOuts)
    zeroOut(trueSOuts)
    
    genProbs.iterator.foreach({
      case (context,outs) => {
        val lambdaC = backoffFromC(context)
        if(lambdaC > 0) { //its a valid full context
          outs.iterator.foreach({
            case (o,c) => {
              val lambda2 = backoffOfromC(context)
              if(lambda2 > 0) {
                val m = trueOuts(context)
                if((m contains o) && lambda2 > 0) {
                  //its a valid full outcome too, and so the pair would be in the initial sweep
                  addmap(o,lambdaC*lambda2*c,m)
                }
              }
              if(lambda2 < 1) {
                val m2 = trueSOuts(context)
                val oS = smoothO(o)
                val myC = lambdaC*(1.0-lambda2)*c
                if(m2 contains oS)
                  addmap(oS,myC,m2)

                if(smoothedOuts contains oS) { 
                  //its a valid sO, but might be new with this context
                  addmap(o,myC,smoothedOuts(oS))
                }
              }
            }
          })
        }

        val cSmooth = smoothC(context)
        
        if((backOuts contains cSmooth) || (backSOuts contains cSmooth)) { //smooth C is valid
          val lambdaSC = backoffOfromSC(cSmooth)
          outs.iterator.foreach({
            case (o,c) => {
              if(lambdaSC > 0) { //ST
                val m = backOuts(cSmooth)
                if(m contains o) {
                  addmap(o,(1.0-lambdaC)*lambdaSC*c,m)
                }
              }
              if(lambdaSC < 1) { //SS
                val m2 = backSOuts(cSmooth)
                val oS = smoothO(o)
                if(m2 contains oS) {
                  addmap(oS,(1.0-lambdaC)*(1.0-lambdaSC)*c,m2)
                }
                if((smoothedOuts contains oS) && (smoothedOuts(oS) contains o)) { 
                  val myC = (1.0 - lambdaC)*(1.0 - lambdaSC)*c
                  addmap(o,myC,smoothedOuts(oS))
                }
              }
            }
          })
        }
      }
    })

  }
*/


}
