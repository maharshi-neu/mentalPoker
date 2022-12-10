import scala.annotation.tailrec
import scala.util.Random
package object util {
  def sqrt(n: Int, q: Int): (Int, Int) = {
    require(n < q)
    for (i <- 1 to q) {
      if i * i % q == n then return (i, q - i)
    }
    throw new Exception("No solution")
  }

  def egdc(a: Int, b: Int): (Int, Int, Int) = {
    if b == 0 then return (a, 1, 0)
    val (g, x, y) = egdc(b, a % b)
    (g, y, x - (a / b) * y)
  }

  def inv(n: Int, q: Int): Int = {
    egdc(n, q)._2 % q
  }

  val rand: Random = new scala.util.Random

  def primitiveRoot(p: BigInt): BigInt = {
    val g = randomNextBigInts(p)
    if (g.modPow(p, p) == 1 || g.modPow(2, p) == 1) primitiveRoot(p)
    g
  }

  def nBitRandom(n: Int): BigInt = {
    val bytes = new Array[Byte](n / 8)
    rand.nextBytes(bytes)
    BigInt(bytes).abs
  }

  @tailrec
  def getNBitRandomPrime(n: Int): BigInt = {
    val p = nBitRandom(n)
    if (p.isProbablePrime(100)) p else getNBitRandomPrime(n)
  }

  def encode(m: String): BigInt = BigInt(m.getBytes)
  def decode(e: BigInt): String = String(e.toByteArray)

  /**
   * workaround for generating random BigInt between a range
   */
  def randomNextBigInts(a: BigInt): BigInt = {
    val rng = rand.between(3, a.toString().length - 1)
    BigInt((1 to rng).scanLeft(rand.nextInt(9) + 1) {
      case (_, _) => rand.nextInt(10)
    }.mkString)
  }
}
