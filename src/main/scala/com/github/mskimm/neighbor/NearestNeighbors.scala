package com.github.mskimm.neighbor

import com.github.mskimm.neighbor.metric.Metric

import scala.collection.generic.{Growable, Shrinkable}

abstract class NearestNeighbors[A](protected val d: Int)
  extends Metric
    with Growable[Item[A]]
    with Shrinkable[Item[A]]
    with Serializable {

  def +=(elem: Item[A]): this.type

  def -=(elem: Item[A]): this.type

  def clear(): Unit

  def neighbors(q: Array[Double], n: Int): Array[Neighbor[A]]

  def iterator: Iterator[Item[A]]

  def contains(elem: Item[A]): Boolean

  def length: Int

  def dim: Int = d

}
