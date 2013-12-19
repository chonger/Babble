import multitool._

object Babble {

  val rando = new java.util.Random()

  def sample[A](l : scala.collection.GenTraversableOnce[(A,Double)]) : A = {
    val r = rando.nextDouble()
    var a = 0.0
    l.foreach(x => {
      val d = x._2
      a += d
      if(a >= r) {
        return x._1
      }
    })
    throw new Exception("Sample Fail")
  }

  def samplePower[A](p : Double, l : scala.collection.GenTraversableOnce[(A,Double)]) : A = {
    val vs = l.toArray
    val pow = vs.map(x => math.pow(x._2,p))
    val tot = (0.0 /: pow)(_ + _)
    sample((vs zip pow).map(x => {
      (x._1._1,x._2 / tot)
    }))
  }


}


abstract class BabbleSyntax {

  def genSentence() : ParseTree

}

class BabbleData(st : CFGSymbolTable, f : String) extends BabbleSyntax {

  val tz = st.read(f).map(x => {
    
    def rrr(n : NonTerminalNode) : List[NonTerminalNode] = {
      n match {
        case in : ProtoNode => {
          if(st.syms(in.symbol).indexOf("@") == 0)
            in.children.flatMap(x => rrr(x))
          else
            List(new ProtoNode(in.symbol,in.children.flatMap(x => rrr(x))))
        }
        case ptn : PreTerminalNode => List(new PreTerminalNode(ptn.symbol,new TerminalNode(st.terms.add("UNK"))))
      }
    }
    
    val rootz = rrr(x.root)
    new ParseTree(rootz(0))
  })
  
  def genSentence() : ParseTree = {
    val ind = Babble.rando.nextInt(tz.length)
    tz(ind)
  }

}

class BabbleSyntaxPTSG(val ptsg : PTSG, val rootSym : String) extends BabbleSyntax {
  
  def this(ptsg_ : PTSG) = {
    this(ptsg_,"ROOT")
  }
  
  val st = ptsg.st
  val rootInd = st.syms.ids(rootSym)
  
  var upGraph = Array.tabulate(st.syms.size)(x => List[((ParseTree,RefWrapper),Double)]())

  0.until(st.syms.size).foreach(i => {
    ptsg.rules(i).iterator.foreach({
      case (rule,prob) => {
        rule.underspecs.foreach(un => {
          upGraph(un.symbol) ::= ((rule,new RefWrapper(un)),prob)
        })
      }
    })
  })

  upGraph = upGraph.map(ll => {
    var tot = (0.0 /: ll)(_ + _._2)
    ll.map(x => (x._1,x._2/tot))
  })

/**
  var c = 0
  upGraph.foreach(x => {
    println("UP FROM " + st.syms(c))
    c += 1
    x.foreach({
      case ((pt,n),p) => println(pt.fString(st) + " --- " + p)
    })
  })
*/

        
  def sampleRule(i : Int) : ParseTree = {
    try { 
      Babble.sample(ptsg.rules(i).iterator)
    } catch {
      case e : Exception => {
        println("failed to sample NT - " + st.syms(i))
        throw e
      }
    }
  }

  def sampleUp(i : Int) : List[(ParseTree,RefWrapper)] = {
    var path = List[(ParseTree,RefWrapper)]()
    var cur = i
    //println("going up from " + st.syms(i))

    while(cur != rootInd) {
      assert(upGraph(i).length != 0,"No options to go up from " + st.syms(i))
      var next = Babble.sample(upGraph(i))
      path ::= next
      cur = next._1.root.symbol
    }
    path
  }


  def recDown(n : NonTerminalNode) : NonTerminalNode = {
    n match {
      case ptn : PreTerminalNode => new PreTerminalNode(ptn.symbol,new TerminalNode(ptn.kid.terminal))
      case pn : ProtoNode => new ProtoNode(pn.symbol,pn.children.map(x => recDown(x)))
      case un : UnderspecifiedNode => genDown(un.symbol).root
    }
  }

  def genDown(i : Int) : ParseTree = {
    val rule = sampleRule(i)
    new ParseTree(recDown(rule.root))
  }

  override def genSentence() : ParseTree= {
    genDown(rootInd)
  }
    
  def genOut(seg : ParseTree) = {
    var path = sampleUp(seg.root.symbol)

    def recOutDown(mypath : List[(ParseTree,RefWrapper)]) : NonTerminalNode = {
      
      if(mypath.length == 0) {
        recDown(seg.root)
      } else {
        val (tree,target) = mypath(0)

        def recDownInner(n : NonTerminalNode) : NonTerminalNode = {
          n match {
            case ptn : PreTerminalNode => ptn
            case pn : ProtoNode => new ProtoNode(pn.symbol,pn.children.map(x => recDownInner(x)))
            case un : UnderspecifiedNode => {
              if(target == new RefWrapper(un)) {
                recOutDown(mypath.drop(1))
              } else {
                genDown(un.symbol).root
              }
            }
          }
        }
        
        recDownInner(tree.root)
      }
    }

    new ParseTree(recOutDown(path))

  }

}
