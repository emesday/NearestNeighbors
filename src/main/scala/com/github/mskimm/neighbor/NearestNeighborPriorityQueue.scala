package com.github.mskimm.neighbor

import com.github.mskimm.collection.BoundedPriorityQueue
import com.github.mskimm.neighbor.Neighbor.{NeighborOrdering, NeighborPriority}

class NearestNeighborPriorityQueue(capacity: Int)
  extends BoundedPriorityQueue[Neighbor[_]](capacity)(NeighborPriority) {

  def result[T]: Array[Neighbor[T]] =
    toArray.sorted(NeighborOrdering).asInstanceOf[Array[Neighbor[T]]]

  def maxPriority: Double =
    underlying.peek() match {
      case null => Double.MaxValue
      case neighbor => neighbor.distance
    }

}
