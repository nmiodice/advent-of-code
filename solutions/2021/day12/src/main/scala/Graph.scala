

class Graph[T] {
  private var edges = Map[T, Set[T]]()

  def addBiDirectionalEdge(a: T, b: T): Unit = {
    addEdge(a, b)
    addEdge(b, a)
  }

  def addEdge(a: T, b: T): Unit = {
    edges = edges + (a -> (edgesFor(a) + b))
  }

  def edgesFor(a: T): Set[T] = {
    edges.getOrElse(a, Set())
  }
}

final class Cave(val id: String) {
  override def equals(obj: Any): Boolean = obj match {
    case x: Cave => id.equals(x.id)
    case _ => false
  }

  override def hashCode(): Int = id.hashCode

  override def toString: String = id

  def isSmallCave: Boolean = id.toLowerCase().equals(id)
}
