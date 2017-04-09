package com.github.mskimm.neighbor

import com.github.mskimm.neighbor.metric.EuclideanMetric

case class KdNode[A](item: Item[A], var left: KdNode[A], var right: KdNode[A], var deleted: Boolean) {

  def toString(depth: Int): String = {
    var s = item.toString + (if (deleted) " *" else "")
    if (left != null)
      s += "\n" + " " * depth + "L " + left.toString(depth + 1)
    if (right != null)
      s += "\n" + " " * depth + "R " + right.toString(depth + 1)
    s
  }

}

case class KdRect(min: Array[Double], max: Array[Double]) {

  require(min.length == max.length)

  def closest(q: Array[Double]): Array[Double] = {
    require(q.length == min.length)
    val r = new Array[Double](q.length)
    var i = 0
    while (i < q.length) {
      if (q(i) <= min(i)) {
        r(i) = min(i)
      } else if (q(i) >= max(i)) {
        r(i) = max(i)
      } else {
        r(i) = q(i)
      }
      i += 1
    }
    r
  }

  override def toString: String = {
    min.mkString(" ") + "\n" + max.mkString(" ") + "\n"
  }

}

object KdRect {

  def inf(d: Int): KdRect = {
    val min = Array.fill[Double](d)(Double.NegativeInfinity)
    val max = Array.fill[Double](d)(Double.PositiveInfinity)
    KdRect(min, max)
  }

}

// Scala implementation of
// https://sourceforge.net/p/java-ml/java-ml-code/ci/master/tree/src/net/sf/javaml/core/kdtree/KDTree.java
class KdTree[A](k: Int) extends NearestNeighbors[A](k)
  with EuclideanMetric {

  private var root: KdNode[A] = _

  private var count: Int = 0

  private def insert(r: KdNode[A], item: Item[A], c: Int): KdNode[A] = {
    r match {
      case null =>
        KdNode(item, null, null, deleted = false)
      case _ if r.item == item =>
        // reinsert
        r.deleted = false
        r
      case _ if item.key(c) > r.item.key(c) =>
        r.right = insert(r.right, item, (c + 1) % k)
        r
      case _ =>
        r.left = insert(r.left, item, (c + 1) % k)
        r
    }
  }

  override def +=(elem: Item[A]): this.type = {
    root = insert(root, elem, 0)
    count += 1
    this
  }

  override def -=(elem: Item[A]): this.type = {
    get(elem) match {
      case null =>
      case found =>
        found.deleted = true
        count -= 1
    }
    this
  }

  override def clear(): Unit = {
    count = 0
    root = null
  }

  def search(r: KdNode[A], q: Array[Double], c: Int, rect: KdRect, distUB0: Double, queue: NearestNeighborPriorityQueue): Unit = {
    if (r == null) {
      return
    }

    val s = c % k

    val pivot = r.item.key
    val distToPivot = sqDist(pivot, q)

    val leftRect = rect
    val rightRect = KdRect(rect.min.clone(), rect.max.clone())
    leftRect.max(s) = pivot(s)
    rightRect.min(s) = pivot(s)

    val (a, aRect, b, bRect) = if (q(s) < pivot(s)) {
      (r.left, leftRect, r.right, rightRect)
    } else {
      (r.right, rightRect, r.left, leftRect)
    }

    search(a, q, c + 1, aRect, distUB0, queue)

    var dist = if(!queue.isFull) {
      Double.MaxValue
    } else {
      val d = queue.maxPriority
      d * d
    }

    var distUB = math.min(distUB0, dist)

    val closest = bRect.closest(q)
    if (sqDist(closest, q) < distUB) {
      if (distToPivot < dist) {
        dist = distToPivot
        if (!r.deleted) {
          queue += Neighbor(r.item, math.sqrt(dist))
        }
        distUB = if (queue.isFull) {
          val d = queue.maxPriority
          d * d
        } else {
          Double.MaxValue
        }
      }

      search(b, q, c + 1, bRect, distUB, queue)
    }

  }

  override def neighbors(q: Array[Double], n: Int): Array[Neighbor[A]] = {
    if (root == null) {
      return Array.empty
    }

    val queue = new NearestNeighborPriorityQueue(n)
    val rect = KdRect.inf(k)
    search(root, q, 0, rect, Double.MaxValue, queue)
    queue.result[A]
  }

  private def get(elem: Item[A]): KdNode[A] = {
    var c = 0
    var r = root
    var found: KdNode[A] = null
    while (r != null && found == null) {
      if (!r.deleted && elem == r.item) {
        found = r
      } else if (elem.key(c) > r.item.key(c)) {
        r = r.right
      } else {
        r = r.left
      }
      c = (c + 1) % k
    }
    found
  }

  override def contains(elem: Item[A]): Boolean = get(elem) != null

  override def length: Int = count

  override def toString: String = {
    if (root == null) {
      "null"
    } else {
      root.toString(0)
    }
  }

  override def iterator: Iterator[Item[A]] = ???

}
