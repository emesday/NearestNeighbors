package com.github.mskimm.collection

import scala.collection.JavaConverters._
import scala.collection.generic.Growable

class BoundedPriorityQueue[A](maxSize: Int)(implicit ord: Ordering[A])
  extends Iterable[A]
    with Growable[A]
    with Serializable {

  protected val underlying = new java.util.PriorityQueue[A](maxSize, ord)

  override def +=(elem: A): this.type = {
    if (underlying.size < maxSize) {
      underlying.offer(elem)
    } else {
      val head = underlying.peek()
      if (head != null && ord.gt(elem, head)) {
        underlying.poll()
        underlying.offer(elem)
      }
    }
    this
  }

  override def clear(): Unit = underlying.clear()

  override def iterator: Iterator[A] = underlying.iterator.asScala

  def length: Int = underlying.size

  def isFull: Boolean =
    length >= maxSize

}


