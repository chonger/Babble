import multitool._
import scala.collection.mutable.{HashMap,HashSet}


/**
 *
 *  C/O independant EM, pruning, and vocab constraining code
 *
 */ 


abstract class Lookup {
  def inLexicon(l : Lexicon) : Boolean
  def hasWord(w : String) : Boolean = false
  def save() : String
}


class CON[A,B](val c : A, val o : B, val i : List[CON[A,B]])

abstract class Babbler[Context <: Lookup,Outcome <: Lookup] {

  type CONode = CON[Context,Outcome]

  /**
   *
   * override these
   * 
   */ 
  def getObservations(t : DepNode) : List[(Context,Outcome)]
  def implies(c : Context, o : Outcome) : List[Context] 
  def smoothImplies(a : Any, o : Outcome) : List[Any] = Nil
  def rootContext : Context 

  def compose(c : Context) : CONode = {
    val o = chooseSmooth(c)
    new CONode(c,o,implies(c,o).map(compose _))
  }
  def generate() : CONode = compose(rootContext) 

  def realize(co : CONode) : String

  //default is no smoothing
  def smoothC(c : Context) : Any = c 
  def smoothO(o : Outcome) : Any = o

  val rawCounts = new HashMap[Context,HashMap[Outcome,Double]]()

  //used in vocab pruning
  val smoothedOuts = new HashMap[Any,HashMap[Outcome,Double]]()
  val backSOuts = new HashMap[Any,HashMap[Any,Double]]()
  val backOuts = new HashMap[Any,HashMap[Outcome,Double]]()
  val trueSOuts = new HashMap[Context,HashMap[Any,Double]]()
  val trueOuts = new HashMap[Context,HashMap[Outcome,Double]]()
  val trueTots = new HashMap[Context,Double]()
  val smoothTots = new HashMap[Any,Double]()

  //used in EM
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

  val probs = new HashMap[Context,HashMap[Outcome,Double]]()
  val cprobs = new HashMap[Context,Double]()

  def save(filename : String) = {
    import java.io._
    val bw = new BufferedWriter(new FileWriter(filename))
    bw.write(cprobs.size + "\n") //number of contexts
    val cMap = new HashMap[Context,Int]() 
    cprobs.foreach({
      case (c,d) => {
        cMap += c -> cMap.size
        bw.write(c.save() + "\n")
        bw.write(d + "\n")
      }
    })
    val oSet = (new HashSet[Outcome]() ++ probs.iterator.map(_._2).flatMap(x => x.iterator.map(_._1))).toList

    bw.write(oSet.length + "\n")
    val oMap = new HashMap[Outcome,Int]() ++ oSet.zipWithIndex
    oSet.foreach(o => {bw.write(o.save() + "\n")})

    probs.foreach({
      case (c,os) => {
        assert(cMap contains c)
        var tot = 0.0
        os.foreach({
          case (o,d) => {
            bw.write(cMap(c) + "\t" + oMap(o) + "\t" + d + "\n")
            tot += d
          }
        })
        assert(Math.abs(tot - 1.0) < .0001)
      }
    })
    bw.close()
  }

  def load(filename : String, makeC : (String) => Context, makeO : (String) => Outcome) = {
    val ostr = io.Source.fromFile(filename).getLines
    val cs = 0.until(Integer.parseInt(ostr.next)).map(i => {
      val s = ostr.next.trim
      val c = makeC(s)
      //println(c)
      //println(s)
      cprobs += c -> ostr.next.trim.toDouble
      c
    }).toList
    val os = 0.until(Integer.parseInt(ostr.next)).map(i => {
      val s = ostr.next.trim
      val o = makeO(s)
      o
    }).toList
    ostr.foreach(x => {
      val ps = x.trim.split("\t")
      val c = cs(ps(0).toInt) 
      val o = os(ps(1).toInt)
      val d = ps(2).toDouble
      val ent = probs.getOrElseUpdate(c,new HashMap[Outcome,Double]())
      ent += o -> d
    })
    probs.foreach({
      case (c,os) => {
        var tot = 0.0
        os.foreach({
          case (o,d) => {
            tot += d
          }
        })
        assert(Math.abs(tot - 1.0) < .0001)
      }
    })
  }
  

  def addmap[A](k : A, dd : Double, m : HashMap[A,Double]) : Unit = {
    var e : Double = m.getOrElse(k, 0.0)
    e += dd
    m += k -> e
  }

  /**
   *
   * fills rawCounts
   * 
   */ 
  def addObservations(trees : Array[DepNode]) = {
    trees.foreach(t => {
      getObservations(t).foreach({
        case (context,outcome) => {
          val gMap = rawCounts.getOrElseUpdate(context,new HashMap[Outcome,Double]())
          val gE = gMap.getOrElse(outcome,0.0) + 1.0
          gMap += outcome -> gE
        }
      })      
    })
  }

  def limitCounts(n : Int) = {
    //println("Limiting raw counts - starting with " + (0 /: rawCounts.iterator)(_ + _._2.size) + " C/O pairs")
    rawCounts.iterator.foreach({
      case (context,outs) => {
        outs.iterator.foreach({
          case (o,c) => {
            if(c < n)
              outs -= o
          }
        })
        if(outs.size == 0)
          rawCounts -= context
      }
    })
    //println("Limiting counts leaves " + (0 /: rawCounts.iterator)(_ + _._2.size) + " C/O pairs")
  }  



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
      //println("TOTAL Expected context counts - " + totEx)
    })
    cur.iterator.foreach({case (c,count) => addmap(c,count,cprobs)})
    normalize(cprobs)
  }


  def getRealProbs(context : Context) : HashMap[Outcome,Double] = {
    val lam = lambda.getOrElse(context,0.0) //we might as for a unseen true context
    val sC = smoothC(context)
    val outs = new HashMap[Outcome,Double]()
    if(lam > 0) { //theres some real prob
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
  
  def setProbs() : Unit = setProbs(true)
  def setProbs(doTopo : Boolean) : Unit = {
    val allContexts = allC //this is all consistent contexts

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
    val goodness = new HashMap[Context,Double]()
    
    if(doTopo) {
      import scala.collection.mutable.ArrayBuffer
      //do topo sort
      var tEdges = new ArrayBuffer[(Context,Context,Double)]()

      tEdges ++= edges.iterator.flatMap(x => x._2.map(y => (x._1,y._1,y._2))).toArray.sortWith(_._3 > _._3)


      def bSearch(st : Int, end : Int) : List[Context] = {
        var half = (st + end) / 2
        if(st + 1 == end)
          half += 1
        //println("BSEARCH - " + st + " - " + end + " - " + half)
        
        val tryTopo = topoSort(tEdges.slice(0,half).toArray)
        if(st == end) {
          //println("using " + end + " out of " + tEdges.length)
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
      //println(allContexts.size)
      //println(tEdges.length)
      val topo = bSearch(0,tEdges.length)

      //println("Calc Goodness - " + topo.length)

      topo.foreach(c => {
        var g = 0.0
        val rp = getRealProbs(c)
        outcomesSmooth(c).foreach(o => {
          g += (rp(o) /: implies(c,o))((a,b) => a * goodness.getOrElse(b,0.0))
        })
        goodness += c -> g
      })
    } 


    allContexts.foreach(c => {
      val oprobs = getRealProbs(c)
      val outs = outcomesSmooth(c)
      val ps = outs.iterator.map(o => {
        val v = (oprobs(o) /: implies(c,o))((a,b) => {
          a * goodness.getOrElse(b,1.0)
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
    getContextProbs(100)
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

  def vocabPrune(lexicon : Lexicon) = {
    
    //println("first pass " + rawCounts.size)
    
    var sContextSet = new HashSet[Any]()
    var sOutcomeSet = new HashSet[Any]()
    var outcomeSet = new HashSet[Outcome]()

    rawCounts.iterator.foreach({
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
    
    //println(sContextSet.size + " smooth contexts")
    //println(sOutcomeSet.size + " smooth outcomes")
    //println(outcomeSet.size + " true outcomes")
    
    smoothedOuts.clear()
    
    backSOuts.clear()
    backOuts.clear()
    
    trueSOuts.clear()
    trueOuts.clear()
    
    val backCmap = new HashMap[Any,List[Context]]()
    
    //get the superset of all connections, which will be pruned down

    rawCounts.iterator.foreach({
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

    //println("counts are counted, starting to prune")

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
      //println("Smooth Contexts dropped = " + dropped + " (" + sContextSet.size + " left)")
    }


    //println("everything in pruned, now remove unreachable stuff")

    //check the assertion and kill unreachable contexts
    val allContexts = allC
    //println("All Contexts : " + allContexts.size)
    val finalSC = new HashSet[Any]() ++ allContexts.map(x => smoothC(x))
    //println("Reachable S-Contexts : " + finalSC.size)

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
    //println("Reachable S-Outcomes : " + finalSO.size + " (" + smoothedOuts.size + ")")

    val finalO = new HashSet[Outcome]()
    finalSO.foreach(x => {
      finalO ++= smoothedOuts(x).iterator.map(_._1)
    })
    //println("Reachable Outcomes : " + finalO.size + " (" + outcomeSet.size + ")")
    
    removeUnreachable(finalSO,smoothedOuts)
    removeUnreachable(allContexts,trueSOuts)
    removeUnreachable(allContexts,trueOuts)
    removeUnreachable(finalSC,backOuts)
    removeUnreachable(finalSC,backSOuts)

  }

  lazy val allC = {

    var allContexts = new HashSet[Context]() + rootContext
    //println("Observed Contexts : " + allContexts.size)
    var possible = new HashSet[Context]() ++ allContexts
    
    var newC = 100 
    while(newC > 0) {
      newC = 0
      var check = possible.iterator.toList
      possible.clear()
      
      check.foreach(c => {
        /**
        //this checks the SC assertion
        if(!((backSOuts contains smoothC(c)) || (backOuts contains smoothC(c)))) {
          println("my smoothC = " + smoothC(c))
          println(c.asInstanceOf[NGramContext].prev)
          println("BAD CONTEXT - " + c)
          assert(false)
        }
        */
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
      
      //println("NEW: " + newC + " - Tot = " + allContexts.size)

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

  def em(nIter : Int) = {

    //init branch probs 
    //make dists for each step on full data
    rawCounts.iterator.foreach({
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

      val ADD_ALPHA = .01

      val ex_OC = full_OC.map(x => (x._1,x._2.map(y => (y._1,ADD_ALPHA))))
      val ex_sOC = full_sOC.map(x => (x._1,x._2.map(y => (y._1,ADD_ALPHA))))
      val ex_OsC = full_OsC.map(x => (x._1,x._2.map(y => (y._1,ADD_ALPHA))))
      val ex_sOsC = full_sOsC.map(x => (x._1,x._2.map(y => (y._1,ADD_ALPHA))))
      val ex_OsO = full_OsO.map(x => (x._1,x._2.map(y => (y._1,ADD_ALPHA))))

      val ex_lambda = lambda.map(x => (x._1,Array(ADD_ALPHA,ADD_ALPHA)))
      val ex_gammaC = gammaC.map(x => (x._1,Array(ADD_ALPHA,ADD_ALPHA)))
      val ex_gammaSC = gammaSC.map(x => (x._1,Array(ADD_ALPHA,ADD_ALPHA)))

      rawCounts.iterator.foreach({
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
              ll += count * scala.math.log(totM)
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

      //println("LOG LIKE: " + ll)

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

    0.until(nIter).foreach(i => {
      //println(i)
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
  
/**
  lazy val inputs = {
    var r = new HashMap[Context,List[(Context,Outcome)]]()
    allC.foreach(c => {
      val os = outcomesSmooth(c)
      os.foreach(o => {
        implies(c,o).foreach(c2 => {
          val e = (c,o) :: r.getOrElse(c2,Nil)
          r += c2 -> e
        })
      })
    })
    r
  }
  */
  lazy val inputs = {
    var r = new HashMap[Context,List[(Context,Outcome)]]()
    probs.foreach({
      case (c,os) => {
        os.keySet.iterator.foreach(o => {
          implies(c,o).foreach(c2 => {
            val e = (c,o) :: r.getOrElse(c2,Nil)
            r += c2 -> e
          })
        })
      }
    })
    r
  }

  def hasStart(w : String) = {
    cprobs.filter(_._1.hasWord(w)).size > 0
  }

  def startContext(w : String) = {
    val poss = new HashMap[Context,Double]()
    cprobs.filter(_._1.hasWord(w)).foreach(x => poss += x._1 -> x._2)
    assert(poss.size > 0)
    normalize(poss)
    Babble.sample(poss.iterator)
  }

  def generateUp(w : String) : CONode = {
    
    var cur = startContext(w)
    
    var cPath = List[(Context,Outcome,Int)]()

    while(cur != rootContext) {

      val poss = new HashMap[(Context,Outcome),Double]()
      inputs(cur).foreach(x => {
        val p = cprobs(x._1) * probs(x._1)(x._2)
        poss += x -> p
      })

      normalize(poss)
      val (c,o) = Babble.sample(poss.iterator)

      val im = implies(c,o)
      val poss0 = 0.until(im.length).filter(ii => im(ii) == cur)
      val i = poss0(Babble.rando.nextInt(poss0.length))

      cPath ::= (c,o,i)
      cur = c
    }
    
    composeChain(cPath)
  }

  def composeChain(chain : List[(Context,Outcome,Int)]) : CONode = {
    val (c,outcome,ii) = chain(0)
    var nextChain = chain.drop(1)
    val im = implies(c,outcome)

    if(nextChain.length > 0) {
      new CONode(c,outcome,0.until(im.length).map(jj => if(jj == ii) composeChain(nextChain) else compose(im(jj))).toList)
    } else {
      new CONode(c,outcome,im.map(compose _))
    }
  }

}
