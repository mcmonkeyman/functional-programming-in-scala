import scala.io.Source

object FunProgInScala {

  def main(args: Array[String]) = {


  def runExercises = {
    `exercise2.1`
    `exercise2.2`
    `exercise2.3`
    `exercise2.4`
    `exercise2.5`
  }

  def `exercise2.1` = {
    def fib(l:Int) = {
      @annotation.tailrec
      def fibRemaingTimes(n:Int, prev:Int, prevprev:Int ):Int = {
       if(n==0) {
         prev
       } else {
         fibRemaingTimes(n-1, prev+prevprev, prev) 
       }
      }
      val result = l match {
        case 0 => 0
        case 1 => 1
        case l => fibRemaingTimes(l, 1, 0)
      }
      result
    }
    run(fib(0))
    run(fib(1))
    run(fib(2))
    run(fib(3))
    run(fib(4))
    run(fib(5))
  }

  def `exercise2.2` = {
    def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
      @annotation.tailrec
      def loop(n: Int): Boolean = {
        if ( n >= as.length -1) true
        else if (!ordered(as(n), as(n+1))) false
        else loop(n+1)
      }
      loop(0)
    }
    run(isSorted(List().toArray, (x:Int, y:Int) => x<=y))
    run(isSorted(List(1).toArray, (x:Int, y:Int) => x<=y))
    run(isSorted(List(1,2).toArray, (x:Int, y:Int) => x<=y))
    run(isSorted(List(1,2,3).toArray, (x:Int, y:Int) => x<=y))
    run(isSorted(List(1,3,2).toArray, (x:Int, y:Int) => x<=y))
    run(isSorted(List(3,2,1).toArray, (x:Int, y:Int) => x>=y))
  }

  def `exercise2.3` = {
    def curry[A, B, C] (f: (A, B) => C): A => (B => C) = (a:A) => f(a,_)
    run(curry( (a:String, b:String) => a+b ) )
  }

  def `exercise2.4` = {
    def uncurry[a, b, c] (f: a  => (b => c)): (a, b) => c = (a:a, b:b) => f(a)(b)
    run(uncurry( (a:String) => { (b:String) => a+b} ) )
  }

  def `exercise2.5` = {
    def compose[A, B, C] (f: B => C, g: A => B): A => C = (a:A) => f(g(a))
    var functOne = { (a:String) => a+"a" }
    var functTwo = { (b:String) => b+"b" }
    run( compose( functOne, functTwo ) )
  }

  def run[R](block: => R): R = {
    val result = time(block)
    println(result)
    result
  }

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }


}
