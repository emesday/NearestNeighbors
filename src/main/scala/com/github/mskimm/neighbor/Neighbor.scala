package com.github.mskimm.neighbor

case class Neighbor[A](item: Item[A], distance: Double) {

  override def toString: String = {
    s"Neighbor(($item), $distance)"
  }
}

object Neighbor {

  trait NeighborOrdering extends Ordering[Neighbor[_]] {
    override def compare(x: Neighbor[_], y: Neighbor[_]): Int =
      java.lang.Double.compare(x.distance, y.distance)
  }

  trait NeighborPriority extends Ordering[Neighbor[_]] {
    override def compare(x: Neighbor[_], y: Neighbor[_]): Int =
      java.lang.Double.compare(y.distance, x.distance)
  }

  implicit object NeighborOrdering extends NeighborOrdering

  implicit object NeighborPriority extends NeighborPriority

}
