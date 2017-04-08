package com.github.mskimm.neighbor

import scala.collection.mutable

abstract class BruteForce[A](d: Int)
  extends NearestNeighbors[A](d) {

  private val underlying = new mutable.ListBuffer[Item[A]]

  override def +=(elem: Item[A]): this.type = {
    underlying += elem
    this
  }

  override def -=(elem: Item[A]): this.type = {
    underlying -= elem
    this
  }

  override def clear(): Unit = underlying.clear()

  override def neighbors(key: Array[Double], n: Int): Array[Neighbor[A]] = {
    val queue = new NearestNeighborPriorityQueue(n)
    underlying foreach { elem =>
      queue += Neighbor(elem, distance(key, elem.key))
    }
    queue.result[A]
  }

  override def iterator: Iterator[Item[A]] = underlying.iterator

  override def length: Int = underlying.length

  override def contains(elem: Item[A]): Boolean =
    underlying.contains(elem)

}

