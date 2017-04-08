package com.github.mskimm.neighbor.metric

import com.github.mskimm.neighbor.NearestNeighbors

trait EuclideanMetric extends Metric {
  self: NearestNeighbors[_] =>

  def sqDist(a: Array[Double], b: Array[Double]): Double = {
    require(a.length == self.d)
    require(b.length == self.d)
    var i = 0
    var sqSum = 0.0
    while (i < a.length) {
      val d =  a(i) - b(i)
      sqSum += d * d
      i += 1
    }
    sqSum
  }

  def distance(a: Array[Double], b: Array[Double]): Double = {
    math.sqrt(sqDist(a, b))
  }

}
