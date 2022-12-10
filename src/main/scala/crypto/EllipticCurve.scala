package crypto

import util.{sqrt, inv, egdc}

case class EllipticCurve(a: Int, b: Int, q: Int) {
  require(a > 0 && b > 0 && a < q && b < q && q > 2)
  require((4 * math.pow(a, 3) + 27 * math.pow(b, 2)) % q != 0)

  val zero: (Int, Int) = (0, 0)

  def isValid(p: (Int, Int)): Boolean = {
    if p == zero then return true
    val l = math.pow(p._2, 2) % q
    val r = (math.pow(p._1, 3) + a * p._1 + b) % q
    l == r
  }

  def at(x: Int): ((Int, Int), (Int, Int)) = {
    require(x < q)
    val ysq = (math.pow(x, 3).toInt + a * x + b) % q
    val (y, my) = sqrt(ysq, q)
    ((x, y), (x, my))
  }

  def neg(p: (Int, Int)) = {
    // require(isValid(p))
    // if p == zero then zero
    (p._1, -p._2 % q)
  }

  def add(p1: (Int, Int), p2: (Int, Int)): (Int, Int) = {
    if p1 == zero then return p2
    if p2 == zero then return p1

    if p1._1 == p2._1 && (p1._2 != p2._2 || p1._2 == 0) then return zero

    if p1._1 == p2._1 then {
      val l = (3 * math.pow(p1._1, 2).toInt + a) * inv(2 * p1._2, q) % q
      val x = Math.floorMod(l * l - p1._1 - p2._1, q)
      val y = Math.floorMod(l * (p1._1 - x) - p1._2, q)
      (x, y)
    }
    else {
      val l = (p2._2 - p1._2) * inv(p2._1 - p1._1, q) % q
      val x = Math.floorMod(l * l - p1._1 - p2._1, q)
      val y = Math.floorMod(l * (p1._1 - x) - p1._2, q)
      (x, y)
    }
  }

  def mul(p: (Int, Int), n: Int): (Int, Int) = {
    var r = zero
    var m2 = p
    var m = n

    while 0 < m do {
      if m % 2 == 1 then r = add(r, m2)
      m2 = add(m2, m2)
      m = m / 2
    }
    r
  }

  def order(g: (Int, Int)): Int = {
    require(isValid(g) && g != zero)

    for (i <- 1 to q + 1) {
      if mul(g, i) == zero then return i
    }
    throw new Exception("Invalid order")
  }
}

object EC

object myMain extends App {
  val ec = EllipticCurve(1, 18, 19)
  val (g, _) = ec.at(7)
  require(ec.order(g) == ec.q)
}
