package examples
import vectorspace._

object BayesianNetwork extends App {
  // Define type synonym: represent probability distribution as boolean vector
  // (sample space consists of true and false)
  type Prob = V[Double, Boolean]
  
  // Define the bases 
  // (in order not to explicitly provide type information all the time)
  val vtrue: Prob = V(true)
  val vfalse: Prob = V(false)
  
  // Helper function to normalize a vector 
  // in case the probabilities don't sum up to one
  def normalized(v: Prob): Prob = {
    val s = v.map(_ => "sum")("sum")
    if (s != 0) v * (1.0/s) else v
  }
  
  // Example network from:
  // https://en.wikipedia.org/wiki/Bayesian_network#Example
  def Rain(): Prob = vtrue*0.2 + vfalse*0.8
  def Sprinkler(r: Boolean): Prob = 
    if (r) (vtrue*0.01 + vfalse*0.99)
    else   (vtrue*0.4  + vfalse*0.6)
  
  def GrassWet(r: Boolean, s: Boolean): Prob =
    if (s)
      if (r) (vtrue*0.99 + vfalse*0.01)
      else   (vtrue*0.9  + vfalse*0.1)
    else
      if (r) (vtrue*0.8 + vfalse*0.2)
      else   vfalse
  
  println("Query 1: Did it rain?")
  val q1 = for {
    r <- Rain()
    s <- Sprinkler(r)
    g <- GrassWet(r,s)
  } yield g
  println("  " + q1)
  
  println("Query 2: Given that the grass is wet, did it rain?")
  val q2 = for {
    r <- Rain()
    s <- Sprinkler(r)
    g <- GrassWet(r,s)
    if g
  } yield r
  println("  " + q2)
  println("  Normalized: " + normalized(q2))
}