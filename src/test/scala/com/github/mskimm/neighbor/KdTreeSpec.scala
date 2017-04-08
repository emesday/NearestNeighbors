package com.github.mskimm.neighbor

import com.github.mskimm.neighbor.metric.EuclideanMetric
import org.scalatest.{FunSuite, Matchers}

import scala.util.Random

class KdTreeSpec extends FunSuite with Matchers {

  val k = 10

  def newPoint(): Array[Double] = Array.tabulate(k)(_ => Random.nextDouble)

  test("KdTree insertion/deletion") {
    val bruteForce = new BruteForce[String](k) with EuclideanMetric
    val kdTree = new KdTree[String](k)

    // insertion
    for (i <- 0 until 100) {
      val item = Item(newPoint(), i.toString)
      bruteForce += item
      kdTree += item
    }

    bruteForce.iterator foreach { item =>
      assert(kdTree.contains(item))
    }

    // deletion
    bruteForce.iterator.toSeq foreach { toDelete =>
      bruteForce -= toDelete
      kdTree -= toDelete
      assert(!kdTree.contains(toDelete))
      bruteForce.iterator foreach { item =>
        assert(kdTree.contains(item))
      }
    }
  }

  test("comparision BruteForce and KdTree") {
    val bruteForce = new BruteForce[String](k) with EuclideanMetric
    val kdTree = new KdTree[String](k)

    for (i <- 0 until 100) {
      val item = Item(newPoint(), i.toString)
      bruteForce += item
      kdTree += item
    }

    for (i <- 0 until 100) {
      val point = newPoint()
      val bruteForceResult = bruteForce.neighbors(point, 3)
      val kdTreeResult = kdTree.neighbors(point, 3)
      assert(kdTreeResult === bruteForceResult)
    }

  }

}
