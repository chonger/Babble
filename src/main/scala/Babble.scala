object Babble {

  val rando = new java.util.Random(517)

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



