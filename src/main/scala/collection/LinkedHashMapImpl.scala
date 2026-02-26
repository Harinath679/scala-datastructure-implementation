package collection

import java.util


class LRU[K, V](capacity: Int)
  extends util.LinkedHashMap[K, V](capacity, 0.75f, true) {

  override def removeEldestEntry(
                                  eldest: util.Map.Entry[K, V]
                                ): Boolean =
    size() > capacity

  /** Get a value by key. Returns Option[V] instead of null. */
  def get(key: K): Option[V] = this.synchronized {
    Option(get(key))
  }

  /** Check if key exists in cache. */
  def contains(key: K): Boolean = this.synchronized {
    containsKey(key)
  }

  /** For debugging / logging. Shows entries in LRU order (least → most recent). */
  override def toString: String = this.synchronized {
    toString
  }
}


object LinkedHashMapImpl extends App {

  val cache = new LRU[String, Int](4)
  cache.put("A", 1)
  cache.put("B", 1)
  cache.put("C", 1)
  cache.put("D", 1)
  cache.put("E", 1)

  println(cache.keySet())

}

