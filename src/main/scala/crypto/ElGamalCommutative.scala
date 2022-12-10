package crypto

import util.{randomNextBigInts, getNBitRandomPrime, primitiveRoot, encode, decode}

/**
 * https://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=6449730
 */
case class ElGamalCommutative(g: BigInt, p: BigInt) {
  private val sk: BigInt = randomNextBigInts(p) // secret key
  private val pk: BigInt = g.modPow(sk, p) // public key
  private val r: BigInt = randomNextBigInts(p) // random number

  def getPublicKey(): BigInt = pk

  def encrypt(m: BigInt): (BigInt, BigInt) = {
    val c1: BigInt = g.modPow(r, p)
    val c2: BigInt = (m * pk.modPow(r, p)) % p
    (c1, c2)
  }

  def decrypt(c: (BigInt, BigInt)): BigInt = {
    val c1: BigInt = c._1
    val c2: BigInt = c._2
    (c2 * c1.modPow(sk, p).modInverse(p)).mod(p)
  }
}

object ElGamalCommutative

object fm extends App {
  val bits = 256
  val plain = "Hello World!"
  val p = getNBitRandomPrime(bits)
  val g = primitiveRoot(p)
  val m = encode(plain)

  println(s"Plain text: $plain")
  println("Encrypting by Alice")
  val alice = ElGamalCommutative(g, p)
  val c1 = alice.encrypt(m)

  println("Encrypting by Bob")
  val bob = ElGamalCommutative(g, p)
  val c2 = bob.encrypt(c1._2)

  println("---")
  println("Decrypting by Alice")
  val m2 = alice.decrypt(c1._1, c2._2)
  println("Decrypting by Bob")
  val m3 = bob.decrypt(c2._1, m2)
  println(s"Decrypted text: ${decode(m3)}")

  println("---")
  println("Decrypting by Bob")
  val m4 = bob.decrypt(c2)
  println("Decrypting by Alice")
  val m5 = alice.decrypt(c1._1, m4)
  println(s"Decrypted text: ${decode(m5)}")
}
