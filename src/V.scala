import scala.collection.immutable.HashMap

class V[K: Numeric, A] private (val bag: HashMap[A, K]) {
  private val num = implicitly[Numeric[K]]
  import num._

  def this() {
    this(HashMap.empty[A, K])
  }

  def this(x: A) {
    this(HashMap(x -> implicitly[Numeric[K]].fromInt(1)))
  }

  def +(that: V[K, A]) = {
    val newBag = bag.merged(that.bag) { case ((x, w1), (_, w2)) => (x, w1 + w2) }
    V(newBag)
  }

  def -(that: V[K, A]) = this + -that

  def *(scalar: K) = V(bag.mapValues { w => w * scalar })
  def unary_-() = this * num.fromInt(-1)

  override def toString() = "V(%s)".format(bag)
}

object V {
  def apply[K: Numeric, A](x: A) = new V[K, A](x)
  def apply[K: Numeric, A](bag: HashMap[A, K]) = new V[K, A](bag)
  def apply[K: Numeric, A](bag: Map[A, K]) = new V[K, A](asHashMap(bag))
  def zero[K: Numeric, A] = new V[K, A]()

  private def asHashMap[K, V](map: Map[K, V]): HashMap[K, V] = {
    HashMap.empty[K, V] ++ map
  }
}

