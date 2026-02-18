package collection

import scala.collection.mutable


object QueueLRUImpl extends App {

  class QueueLRU[K, V](capacity: Int) {

    private val map: mutable.Map[K, V] = mutable.HashMap[K, V]()
    private val queue: mutable.Queue[K] = mutable.Queue[K]()

    def get(key: K): Option[V] = {
      map.get(key).map { value =>
        queue.enqueue(key) // mark recent
        value
      }
    }

    def put(key: K, value: V): Unit = {
      map.put(key, value)
      queue.enqueue(key)

      if (map.size > capacity)
        evict()
    }

    private def evict(): Unit = {
      while (queue.nonEmpty) {
        val oldest = queue.dequeue()
        if (map.contains(oldest)) {
          map.remove(oldest)
          return
        }
        // else stale entry → skip
      }
    }

    def currentKeys: List[K] = map.keys.toList
  }


  val cache = new QueueLRU[String, Int](3)

  cache.put("A",1)
  cache.put("B",2)
  cache.put("C",3)

  cache.get("B")   // B becomes recent
  cache.put("D",4) // evicts A

  println(cache.currentKeys)
  // List(B, C, D)

  cache.put("E", 5)
  println(cache.currentKeys)
  // List(C, D, E)


}
