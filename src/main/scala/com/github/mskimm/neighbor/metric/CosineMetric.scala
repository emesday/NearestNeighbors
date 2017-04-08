package com.github.mskimm.neighbor.metric

import com.github.mskimm.neighbor.NearestNeighbors

trait CosineMetric extends Metric {
  self: NearestNeighbors[_] =>

  private def cosineSimilarity(a: Array[Double], b: Array[Double]): Double = {
    var i = 0
    var dot = 0.0
    var na = 0.0
    var nb = 0.0
    while (i < a.length) {
      na += a(i) * a(i)
      nb += b(i) * b(i)
      dot += a(i) * b(i)
      i += 1
    }
    if (na > 0 && nb > 0) {
      dot / math.sqrt(na) / math.sqrt(nb)
    } else {
      0
    }
  }

  def distance(a: Array[Double], b: Array[Double]): Double = {
    require(a.length == self.d)
    require(b.length == self.d)
    2 - cosineSimilarity(a, b)
  }

}
