import multitool._
import scala.collection.mutable.{HashSet,HashMap}

object Main {

  def usage() = {
    println("USAGE [Argument Value]*")
    println()
    println("Available command line arguments")
    println()
    println("PRIMARY - either -in or -tb must be specified but not both")
    println("-in                    :  full path to input flat file")
    println("-tb                    :  full path for output of parsed file")
    println("-out                   :  full path for output of model (.bab) file")

    println("OPTIONAL (default in [] will be used if not specified")
    println()
    println("    Argument           :        Value     ")
    println()
    println("-save                  :  full path for split and parsed sentences")
    println("                          if not specified then trees will not be saved")
    println()
    println("-sent                  :  sentence boundary detection.  Possible values are")
    println("                               NONE - treat each input line as a sentence")
    println("                               PART - honor existing newlines as boundaries")
    println("                               [FULL] - concantenate all text and find new splits")
    println()
    println("-filter                :  filter sentences after parsing")
    println("                               [NONE] - do not filter")
    println("                               SNPVP - keep sentences that parse to a top level S")
    println("                                         with an NP and a VP child")
    println()
    println("-fullEst               :  no value needed, if specified then the full approximation of vocab probs is used")
    println()
    println("-smooth                :  value is a positive integer X")
    println("                          discard any context - outcome pair that occurs less than X times")
    println()
    println("-vocabType             :  the type of vocab to be used, needs to be used with vocabArg")
    println("                               [COUNT] - use the most common (vocabArg) words")
    println("                               FILE - use a vocab from a file")
    println("-vocabArg              :  depends on vocabType")
    println("                               if vocabType = COUNT, the size of the vocab")
    println("                               if vocabType = FILE, the full path to the vocab")
    println()
    println("-writeOrig             :  full path for output of segmented and tokenized sentences")
    println("                          default is to not output this file")

    System.exit(-2)
  }

  def main(args : Array[String]) : Unit = {


    var argz = new HashMap[String,String]()

    val iter = args.iterator
    
    var load : Boolean = false
    var input : String = ""
    var tb : String = ""
    var output : String = ""
    var cc = 0

    

    while(iter.hasNext) {
      iter.next() match {
        case "Main" => {
          //its ok...
        }
        case "-in" => {
          input = iter.next()
          load = false
          cc += 1
          if(cc > 1) {
            println("Please do not define both -tb and -in")
            usage()
          }
        }
        case "-out" => {
          output = iter.next()
        }
        case "-tb" => {
          tb = iter.next()
          load = true
          cc += 1
          if(cc > 1) {
            println("Please do not define both -tb and -in")
            usage()
          }
        }
        case "-save" => {
          tb = iter.next()
        }
        case "-sent" => {
          val x = iter.next()
          if(List("FULL","PART","NONE") contains x)
            argz += "SENTD" -> x
          else {
            usage()
          }
        }
        case "-filter" => {
          val x = iter.next()
          if(List("SNPVP","NONE") contains x)
            argz += "filter" -> x
          else {
            usage()
          }
        }
        case "-smooth" => {
          val x = iter.next()
          if(x.toInt > 0)
            argz += "smooth" -> x
          else {
            usage()
          }
        }
        case "-vocabType" => {
          val x = iter.next()
          if(List("COUNT","FILE") contains x)
            argz += "vocabType" -> x
          else {
            usage()
          }
        }
        case "-vocabArg" => {
          argz += "vocabArg" -> iter.next()
        }
        case "-writeOrig" => {
          argz += "writeOrig" -> iter.next()
        }
        case "-fullEst" => {
          argz += "fullEst" -> ""
        }

        case x : String  => {
          println("Failed to parse command line argument " + x)
          usage()
        } 
      }
    }

    try {
      if(load) {
        Ingest.loadThenBuild(tb,output,argz)
      } else {
        if(tb != "") {
          Ingest.parseThenSave(input,tb,argz)
          Ingest.loadThenBuild(tb,output,argz)
        } else {
          Ingest.parseThenBuild(input,output,argz)
        } 
      }
    } catch {
      case e : Exception => {
        e.printStackTrace()
        usage()
      }
    }
  }
   
}
