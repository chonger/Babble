class PWContext(val pTag : Int, val pW : Int,val cur : (Int,Int)) {
  override def equals(any : Any) : Boolean = {
	any match {
	  case t : PWContext => pTag == t.pTag && pW == t.pW && cur == t.cur
	  case _ => false
	}	
  }
  override def hashCode() : Int = cur.hashCode() ^ pTag.hashCode() ^ pW.hashCode()
}

/**
  def smoothImply(c : Any, o : Any) : List[Any] = {
    o.asInstanceOf[List[Int]].flatMap(x => {
      if(x == -1)
        List[Any]()
      else
        List(x :: c.asInstanceOf[List[Int]])
    })
  }
*/

  /**
  def pruneSmooth() = {

    //an outcome is not allowed for a if it creates a new smooth context that doesnt exist
    
    import scala.collection.mutable.HashSet

    val allSmooth = new HashMap[Any,HashSet[Any]]()
    genProbs.iterator.foreach({
      case (context, outcomes) => {
        val dd = smoothC(context)
        outcomes.foreach(x => {
          val e = allSmooth.getOrElse(dd,new HashSet[Any]) 
          e += smoothO(x._1).asInstanceOf[Any] 
          allSmooth += dd -> e
        }) 
      }
    })

    var changed = 100
    while(changed > 0) {
      changed = 0
      var empty = new HashSet[Any]()
      allSmooth.foreach({
        case (c,os) => {
          os.foreach(o => {
            
            val imps = smoothImply(c,o)

            var ok = true
            imps.foreach(im => {
              if(!(allSmooth contains im)) {
                ok = false
              }
            })
  
            if(!ok) {
              changed += 1
              os -= o
              if(os.size == 0) {
                empty += c
              }
            }
            
          })

        }
      })
      
      empty.foreach(x => {
        allSmooth -= x
      })

      var bad = List[PLContext]()
      genProbs.iterator.toArray.foreach(x => {
        if(empty contains smoothC(x._1)) {
          //println("Remove Context " + smoothC(x._1))
          bad ::= x._1
        }
      })
      
      bad.foreach(b => {
        genProbs -= b
      })
      
      println("Removed " + changed + " generations")
    }


    var finals = List[(PLContext,AllOutcome,Double)]()
    genProbs.iterator.foreach({
      case (context,os) => {
        val sC = smoothC(context)
        os.foreach({
          case (o,d) => {
            val sO = smoothO(o)
            if((allSmooth contains sC) && (allSmooth(sC) contains sO)) {
              finals ::= (context,o,d)
            }
          }
        })
      }
    })

    genProbs.clear()
    finals.foreach({
      case (gk,out,d) => {
        val m = genProbs.getOrElseUpdate(gk,new HashMap[AllOutcome,Double]())
        m += out -> d
      }
    })
    
    println("at the end we have " + unique())

  }
*/
