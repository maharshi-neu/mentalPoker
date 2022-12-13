package simulation

import crypto.ElGamalCommutative
import util.{getNBitRandomPrime, primitiveRoot, encode, decode}
import scala.util.Random
import scala.language.postfixOps

// 2 through Ace
val cards: List[Int] = List(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)

object play extends App {
  val bits = 256
  val p = getNBitRandomPrime(bits)
  val g = primitiveRoot(p)

  val tom = ElGamalCommutative(g, p)
  val dick = ElGamalCommutative(g, p)
  val harry = ElGamalCommutative(g, p)

  val tomEnc = cards.map(c => tom.encrypt(c))
  val shuffledTomEnc = Random.shuffle(tomEnc)

  val dickTomEnc = shuffledTomEnc.map(c => dick.encrypt(c._2))
  val shuffledDickTomEnc = Random.shuffle(dickTomEnc)

  val harryDickTomEnc = shuffledDickTomEnc.map(c => harry.encrypt(c._2))
  val shuffledHarryDickTomEnc = Random.shuffle(harryDickTomEnc)


  val harryRandom = shuffledHarryDickTomEnc(0)._1
  val dickRandom = shuffledDickTomEnc(0)._1
  val tomRandom = shuffledTomEnc(0)._1

  // harry picks a card to give to tom, decrypts it and passes it to dick
  val harryCardEncTomDick = shuffledHarryDickTomEnc.head
  val passedByHarry = shuffledHarryDickTomEnc.tail

  val harryCardEncTom = dick.decrypt(dickRandom, harryCardEncTomDick._2)
  val harryCardEnc = tom.decrypt(tomRandom, harryCardEncTom)
  val harryCard = harry.decrypt(harryRandom, harryCardEnc)

  // dick picks a card to give to harry, decrypts it and passes it to tom
  val dickCardEncHarryTom = passedByHarry.head
  val passedByDick = passedByHarry.tail

  val dickCardEncHarry = tom.decrypt(tomRandom, dickCardEncHarryTom._2)
  val dickCardEnc = harry.decrypt(harryRandom, dickCardEncHarry)
  val dickCard = dick.decrypt(dickRandom, dickCardEnc)

  // tom picks a card to give to dick, decrypts it and passes it to harry
  val tomCardEncDickHarry = passedByDick.head
  //val passedByTom = passedByDick.tail

  val tomCardEncDick = harry.decrypt(harryRandom, tomCardEncDickHarry._2)
  val tomCardEnc = dick.decrypt(dickRandom, tomCardEncDick)
  val tomCard = tom.decrypt(tomRandom, tomCardEnc)


  println(s"Harry's card = " + harryCard)
  println(s"Dick's card = " + dickCard)
  println(s"Tom's card = " + tomCard)

  if (harryCard > dickCard && harryCard > tomCard) println("Harry Won!")
  else if(dickCard > tomCard) println("Dick Won!")
  else println("Tom Won!")

  /**
   * Output:
   * Harry's card = 12
   * Dick's card = 11
   * Tom's card = 5
   * Harry Won!
   */

}
