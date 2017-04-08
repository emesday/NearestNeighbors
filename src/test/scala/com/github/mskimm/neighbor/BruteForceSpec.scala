package com.github.mskimm.neighbor

import com.github.mskimm.neighbor.metric.{CosineMetric, EuclideanMetric}
import org.scalatest.{FunSuite, Matchers}

class BruteForceSpec extends FunSuite with Matchers {

  // http://www.geeksforgeeks.org/k-dimensional-tree/
  val k = 2

  val points = Array(
    Item(Array[Double]( 3 , 6), "a"),
    Item(Array[Double](17, 15), "b"),
    Item(Array[Double](13, 15), "c"),
    Item(Array[Double]( 6, 12), "d"),
    Item(Array[Double]( 9,  1), "e"),
    Item(Array[Double]( 2,  7), "f"),
    Item(Array[Double](10, 19), "g")
  )

  test("BruteForce with CosineMetric") {
    val nns = new BruteForce[String](k) with CosineMetric ++= points
    assert(nns.contains(points(0)))
    assert(nns.neighbors(points(0).key, 1).map(_.item) contains points(0))
    assert(nns.neighbors(points(0).key, 2).map(_.item) contains points(3))
    assert(nns.neighbors(points(0).key, 3).map(_.item) contains points(6))

    nns -= points(0)

    assert(!nns.contains(points(0)))
    assert(nns.neighbors(points(0).key, 1).map(_.item) contains points(3))
    assert(nns.neighbors(points(0).key, 2).map(_.item) contains points(6))
    assert(nns.neighbors(points(0).key, 3).map(_.item) contains points(5))
  }

  test("BruteForce with EuclideanMetric") {
    val nns = new BruteForce[String](k) with EuclideanMetric ++= points
    assert(nns.contains(points(0)))
    assert(nns.neighbors(points(0).key, 1).map(_.item) contains points(0))
    assert(nns.neighbors(points(0).key, 2).map(_.item) contains points(5))
    assert(nns.neighbors(points(0).key, 3).map(_.item) contains points(3))

    nns -= points(0)

    assert(!nns.contains(points(0)))
    assert(nns.neighbors(points(0).key, 1).map(_.item) contains points(5))
    assert(nns.neighbors(points(0).key, 2).map(_.item) contains points(3))
    assert(nns.neighbors(points(0).key, 3).map(_.item) contains points(4))
  }

}
