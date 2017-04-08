package com.github.mskimm.neighbor

case class Item[A](key: Array[Double], props: A) {

  override def toString: String = {
    val sample = key.take(3).mkString(", ")
    if (key.isEmpty) {
      s"Item((0), $props)"
    } else if (key.length <= 3) {
      s"Item((${key.length}, $sample), $props)"
    } else {
      s"Item((${key.length}, $sample, ...), $props)"
    }
  }

}
