import multitool._
import scala.collection.mutable.HashMap



class BabbleLex {

  def pickWord(item : RefWrapper, 
               chains : HashMap[RefWrapper,List[NonTerminalNode]],
               parentChain : HashMap[RefWrapper,RefWrapper],
               lexes : HashMap[RefWrapper,String]) : String = {
    return "word"
  }

  def lexicalize(st : CFGSymbolTable, t : ParseTree) = {

    val hf = new HeadFinder(st,t)

    val (chains,parentChain) = getChains(st,t,hf)
    val lexes = new HashMap[RefWrapper,String]

    chains.iterator.foreach({
      case (ptn,ch) => {
        val s = st.terms(ptn.n.asInstanceOf[PreTerminalNode].kid.terminal)
        if(s != "UNK") {
          lexes(ptn) = s
        }
      }
    })


    
    var proc = List[RefWrapper]()
    proc ::= new RefWrapper(hf(t.root))
    
    var c = 0
    var l = t.terminals.length


    while(proc.length > 0) {
      c += 1
      if(c > l)
        throw new Exception()
      val item = proc(0)
      proc = proc.drop(1)
      doLex(item)
    }

    def doLex(item : RefWrapper) : Unit = {

      //println("do lex for " + item.n.toString(st))

      val chain = chains(item)
      val pChain = parentChain.getOrElse(item,null)

      if(lexes contains item) {
        //do nothing!
      } else {
        //pick the word
        val w = pickWord(item,chains,parentChain,lexes)
        lexes += item -> w
      }

      chain.foreach(node => {
        node match {
          case ptn : PreTerminalNode => {}
          case pn : ProtoNode => {
            pn.children.foreach(c => {
              val cHead = new RefWrapper(hf(c))
              if(cHead != item) {
                proc ::= cHead
              }
            })
          }
        }
      })
    }

    
    def recForm(n : NonTerminalNode) : NonTerminalNode = {
      n match {
        case ptn : PreTerminalNode => {
          new PreTerminalNode(ptn.symbol,new TerminalNode(st.terms.add(lexes(new RefWrapper(ptn)))))
        }
        case pn : ProtoNode => new ProtoNode(pn.symbol,pn.children.map(x => recForm(x)))
      }
    }
    
    new ParseTree(recForm(t.root))

  }
  
  
  
  def getChains(st : CFGSymbolTable, t : ParseTree, hf : HeadFinder) = {   

    var chains = new HashMap[RefWrapper,List[NonTerminalNode]]()
    var parentChain = new HashMap[RefWrapper,RefWrapper]()

    def getChainsR(n : NonTerminalNode) : Unit = {
      
      val head = new RefWrapper(hf(n))
      val entry = n :: chains.getOrElse(head,List[NonTerminalNode]())
      chains += head -> entry

      n match {
        case ptn : PreTerminalNode => {
          //nothing
        }
        case pn : ProtoNode => {
          pn.children.foreach(x => {
            val cHead = new RefWrapper(hf(x))
            if(cHead != head) {
              parentChain += cHead -> head
            }
            getChainsR(x)
          })
        }
      }

    }

    getChainsR(t.root)
/**    
    println(t.pString(st))

    chains.foreach({
      case (rw,ns) => {
        val p = parentChain.getOrElse(rw,null)
        println("CHAIN FOR - " + rw.n.toString(st))
        if(p != null)
          println("Parent = " + p.n.toString(st))
        ns.foreach(n => {
          println(n.toString(st))
        })
        println()
      }
    })

    readLine()
   */ 
    (chains,parentChain)
  }

}


class SimpleBabbleLex(val pcfg : PTSG) extends BabbleLex {
  
  override def pickWord(item : RefWrapper, 
               chains : HashMap[RefWrapper,List[NonTerminalNode]],
               parentChain : HashMap[RefWrapper,RefWrapper],
               lexes : HashMap[RefWrapper,String]) : String = {
    val s = item.n.symbol
    val r = Babble.sample(pcfg.rules(s).iterator)
    pcfg.st.terms(r.root.asInstanceOf[PreTerminalNode].kid.terminal)
  }

}


object BabbleLex extends BabbleLex {

  def main(args : Array[String]) : Unit = {
    
    val st= new CFGSymbolTable() 

    val t = st.growTree("(ROOT (NP (DT an) (JJ old) (NN saint)))")

    val hf = new HeadFinder(st,t)

    t.nonterminals.foreach(n => {
      println(n.toString(st))
      println(hf(n).toString(st))
    })

  }

}

