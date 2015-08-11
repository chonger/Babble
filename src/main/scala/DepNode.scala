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
        List(readL(t,st))
      } catch {
        case e : Throwable => {
          println("Failed on " + l)
          throw e
          List[DepNode]()
        }
      }
    }).toArray
  }


  def readL(t : ParseTree, st : CFGSymbolTable) : DepNode = {
    
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
    
    
    new DepNode(postags.add("ROOT"), 
                words.add("ROOT"),
                List[DepNode](form(t.root)),
                List[DepNode]())
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

}

