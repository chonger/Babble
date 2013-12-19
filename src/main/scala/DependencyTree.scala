import multitool._

class DepNode(val pos : Int, val w : Int, val left : List[DepNode], val right : List[DepNode]) {

  def under() : List[DepNode] = {
    left.flatMap(_.under()) ::: List(this) ::: right.flatMap(_.under())
  }

  def print() = {
    under().foreach(x => {
      println()
      println(DepNode.postags(x.pos) + "/" + DepNode.words(x.w))
      println("L")
      x.left.foreach(y => {
        println(DepNode.postags(y.pos) + "/" + DepNode.words(y.w))
      })
      println("R")
      x.right.foreach(y => {
        println(DepNode.postags(y.pos) + "/" + DepNode.words(y.w))
      })
    })
  }

  def sentence() : String = {
    var ret = right.map(x => x.sentence())
    if(DepNode.postags(pos) != "ROOT") 
      ret ::= DepNode.words(w)
    ret :::= left.map(x => x.sentence())
    ret.toArray.mkString(" ")
  }

  def tags() : String = {
    var ret = right.map(x => x.sentence())
    if(DepNode.postags(pos) != "ROOT") 
      ret ::= DepNode.postags(pos)
    ret :::= left.map(x => x.sentence())
    ret.toArray.mkString(" ")
  }

  def seq() : List[(Int,Int)] = {
    val it = (pos,w)
    var ret = right.flatMap(x => x.seq())
    if(DepNode.postags(pos) != "ROOT")
      ret ::= it
    ret :::= left.flatMap(x => x.seq())
    ret
  }

}

object DepNode {

  val postags = new SymbolTable()
  val words = new SymbolTable()

  def read(file : String) : Array[DepNode] = {
    val st = new CFGSymbolTable()
    io.Source.fromFile(file).getLines.flatMap(l => {
      try {
        val t = st.growTree(l.trim)
        val hf = new HeadFinder(st,t)

        def form(n : NonTerminalNode) : DepNode = {
          
          val head = hf(n)

          var lefts = List[DepNode]()
          var rights = List[DepNode]()
          
          def downChain(n : NonTerminalNode) : Unit = {
            n match {
              case pn : ProtoNode => {
                var cInd = 0
                var hInd = 0
                pn.children.foreach(c => {
                  val cHead = hf(c)
                  if(cHead eq head) {
                    downChain(c)
                    hInd = cInd
                  }
                  cInd += 1
                })

                var ind = 0
                pn.children.foreach(c => {
                  if(ind < hInd)
                    lefts ::= form(c)
                  if(ind > hInd)
                    rights ::= form(c)
                  ind += 1
                })

              }
              case ptn : PreTerminalNode => {
                //do nothing?
              }
            }
            
          }

          downChain(n)

          new DepNode(postags.add(st.syms(head.symbol)),
                      words.add(st.terms(head.kid.terminal).toLowerCase()),
                      lefts.reverse,
                      rights.reverse)

        }
        
        
        val ret = new DepNode(postags.add("ROOT"), 
                              words.add("ROOT"),
                              List[DepNode](form(t.root)),
                              List[DepNode]())
        List[DepNode](ret)
      } catch {
        case _ => {
          println("Failed on " + l)
          List[DepNode]()
        }
      }
    }).toArray
  }


  def makeTex(filE : String, dz : Array[DepNode]) = {

    import java.io._
    import scala.collection.mutable.HashMap

    val bw = new BufferedWriter(new FileWriter(filE))

    bw.write("\\documentclass[11pt]{article}\n")
    bw.write("\\usepackage{tikz}\n")
    bw.write("\\usepackage{tikz-dependency}\n")
    bw.write("\\usepackage{booktabs}\n")
    bw.write("\\usepackage{amsmath}\n")
    bw.write("\\usepackage[margin=0.5in]{geometry}\n")
    bw.write("\\begin{document}\n")
    
    dz.foreach(d => {
      
      val nodez = d.under()
      val m = new HashMap[DepNode,Int]() ++= nodez.zipWithIndex

      bw.write("\\begin{equation*}\n")
      var sepsize = ".5"
      if(nodez.length > 13) {
        bw.write("\\tiny\n")
        sepsize = ".2"
      } 


      bw.write("\\begin{dependency}[theme = simple, label style={font = \\bfseries}]\n")
      bw.write("\\begin{deptext}[column sep=" + sepsize + "cm, row sep=0]\n")

      bw.write(nodez.map(x => DepNode.postags(x.pos).replaceAll("\\$","S")).toArray.mkString(" \\& ") + " \\\\\n")
      bw.write(nodez.map(x => DepNode.words(x.w)).toArray.mkString(" \\& ") + " \\\\\n")

      bw.write("\\end{deptext}\n")

      nodez.foreach(n => {
        val myind = m(n) + 1
        (n.left ::: n.right).foreach(nn => {
          val kind = m(nn) + 1
          bw.write("\\depedge{" + myind + "}{" + kind + "}{}\n")
        })
      })

      bw.write("\\end{dependency}\n")
      bw.write("\\end{equation*}\n")
  
    })

    bw.write("\\end{document}\n")

    bw.close()

  }

  def setup() = {
    val lexicon = new Lexicon("/home/chonger/data/generate/vocab/A1_words.txt")
    val tz = DepNode.read("/home/chonger/data/generate/simplewiki/simplewiki2.ptb")
    val dg = new AllDepsL()
    dg.addObservations(tz)
  }



}

class DepGram {
  
  import scala.collection.mutable.HashMap

  class GenKey(val pTag : Int, val cur : (Int,Int),val last : (Int,Int), val dir : Boolean) {
    override def equals(any : Any) : Boolean = {
	  any match {
	    case t : GenKey => pTag == t.pTag && cur == t.cur && last == t.last && dir == t.dir
	    case _ => false
	  }	
    }
    override def hashCode() : Int = cur.hashCode() ^ last.hashCode() ^ pTag.hashCode()
    override def toString() : String = {
      var r = if(dir) "L " else "R "
      if(cur._1 == -1)
        r += "-1"
      else
        r += DepNode.words(cur._2)
      r += " -- "
      if(last._1 == -1)
        r += "-1"
      else
        r += DepNode.words(last._2)
      r
    }
  }


  val genProbs = new HashMap[GenKey,HashMap[(Int,Int),Double]]()

  def limit(lexicon : Lexicon) = {

    val uniq = (0 /: genProbs.iterator)(_ + _._2.size)
    println("starting with " + uniq)

    var good = List[(GenKey,(Int,Int),Double)]()

    genProbs.iterator.foreach({
      case (gK,m) => {
        val curW = DepNode.words(gK.cur._2)
        if(curW == "ROOT" || lexicon.inVocab(curW)) {
          if(gK.last._2 == -1 || lexicon.inVocab(DepNode.words(gK.last._2))) {
            m.iterator.foreach({
              case (out,d) => {
                if(out._2 == -1 || lexicon.inVocab(DepNode.words(out._2))) {
                  if(d > 1)
                    good ::= (gK,out,d)
                }
              }
            })
          }
        }
      }
    })

    genProbs.clear()
    good.foreach({
      case (gk,out,d) => {
        val m = genProbs.getOrElseUpdate(gk,new HashMap[(Int,Int),Double]())
        m += out -> d
      }
    })

    println("Now it's " + good.length)
    
    var changed = 100
    while(changed > 0) {
      changed = 0

      var empty = List[GenKey]()
      genProbs.foreach({
        case (gK,m) => {
          m.iterator.foreach({
            case (out,d) => {
              //if this context generated this outcome
              //if outcome is STOP then no new gens are made
              //otherwise 3 gens are made
              
              if(out != (-1,-1)) {
                //the kid will generate down
                val gk1 = new GenKey(gK.cur._1,out,(-1,-1),true)
                val gk2 = new GenKey(gK.cur._1,out,(-1,-1),false)
                
                // we will continue the chain
                val gk3 = new GenKey(gK.pTag,gK.cur,out,gK.dir)
                
                if((genProbs contains gk1) &&
                   (genProbs contains gk2) &&
                   (genProbs contains gk3)) {
                     //ok
                } else {
                  //bad
                  changed += 1
                  m -= out
                  if(m.size == 0) {
                    empty ::= gK
                  }
                }
              }
            }
          })
        }
      })
      
      empty.foreach(e => genProbs -= e)
      println("Removed " + changed + " generations")
    }

    val uniqend = (0 /: genProbs.iterator)(_ + _._2.size)
    println("at the end we have " + uniqend)
    
    normalize()

  }

  def train(trees : Array[DepNode]) = {
    genProbs.clear()
    addObservations(trees)
    normalize()
  }

  def addObservations(trees : Array[DepNode]) = {

    trees.foreach(t => {

      val pMap = new HashMap[DepNode,DepNode]()
      
      t.under().foreach(n => {
        (n.left ::: n.right).foreach(nn => {
          pMap += nn -> n
        })
      })

      t.under().foreach(n => {
        collect(n.left.reverse,Nil,true)
        collect(n.right,Nil,false)

        def collect(x : List[DepNode], made : List[DepNode],dir : Boolean) : Unit = {
          val outcome = if(x.length == 0) {
            //observe a stop!
            (-1,-1)
          } else {
            (x(0).pos,x(0).w)
          }

          val last = if(made.length == 0) {
            (-1,-1)
          } else {
            (made(0).pos,made(0).w)
          }

          val gPar = pMap.getOrElse(n,null)
          val pT = if(gPar == null) -1 else gPar.pos

          val gKey = new GenKey(pT,(n.pos,n.w),last,dir)
          val gMap = genProbs.getOrElseUpdate(gKey,new HashMap[(Int,Int),Double]())
          val gE = gMap.getOrElse(outcome,0.0) + 1.0
          gMap += outcome -> gE

          if(x.length > 0)
            collect(x.drop(1),x(0) :: made,dir)
        }
      })
    })
  }

  def normalize() = {
    //normalize
    genProbs.iterator.foreach({
      case (k,m) => {
        //println(k)
        val tot = (0.0 /: m.iterator)(_ + _._2)
        m.iterator.foreach({
          case (kk,vv) => m += kk -> vv/tot
        })
      }
    })


  }


  def generate() : DepNode = {
    generate((DepNode.postags.add("ROOT"),DepNode.words.add("ROOT")),-1)
  }

  def generate(c : (Int,Int), pT : Int) : DepNode = {

    var nKids = 0

    def genDeps(x : (Int,Int),d : Boolean) : List[(Int,Int)] = {

      nKids += 1
      if(nKids > 30)
        throw new Exception()

      val k = new GenKey(pT,(c._1,c._2),(x._1,x._2),d)
      //println(k)
      val res = Babble.sample(genProbs(k).iterator)
      if(res == (-1,-1))
        List[(Int,Int)](res)
      else
        res :: genDeps(res,d)
    }

    var left = genDeps((-1,-1),true).reverse.drop(1)
    var right = genDeps((-1,-1),false).reverse.drop(1).reverse
    /**
    println()
    println("Done for " + c)
    println("L - " + left.map(x => DepNode.words(x._2)).toArray.mkString(" "))
    println("R - " + right.map(x => DepNode.words(x._2)).toArray.mkString(" "))
    */
    new DepNode(c._1,c._2,left.map(x => generate(x,c._1)),right.map(x => generate(x,c._1)))
  }



}
