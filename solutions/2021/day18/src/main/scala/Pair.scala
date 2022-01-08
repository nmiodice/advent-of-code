trait Component {
  def tryExplode(parents: List[Component] = List()): Boolean

  def trySplit(parent: Component): Boolean

  def magnitude: BigInt

  def copy: Component
}

class NumericComponent(var value: Int) extends Component {

  def add(i: Int): Unit = {
    value = value + i
  }

  override def tryExplode(parents: List[Component] = List()): Boolean = false

  override def trySplit(parent: Component): Boolean = {
    if (value >= 10) {
      val newPair = new Pair(
        new NumericComponent(value / 2),
        new NumericComponent((value + 1) / 2)
      )

      // replace with the right split pair
      val parentPair = parent.asInstanceOf[Pair]
      if (parentPair.left == this) {
        parentPair.left = newPair
      } else {
        parentPair.right = newPair
      }

      return true
    }

    false
  }

  override def magnitude: BigInt = BigInt(value)

  override def copy: Component = new NumericComponent(value)

  override def toString: String = f"$value"
}

class Pair(var left: Component, var right: Component) extends Component {
  override def toString: String = f"[$left,$right]"

  def reduce(): Unit = {
    while (tryExplode() || trySplit(this)) {
    }
  }

  override def trySplit(parent: Component): Boolean = {
    left.trySplit(this) || right.trySplit(this)
  }

  def tryExplode(parentComponents: List[Component] = List()): Boolean = {
    val parents = parentComponents.map(x => x.asInstanceOf[Pair])
    if (parents.size == 4) {

      // this should always happen according to the problem statement
      assert(left.isInstanceOf[NumericComponent] && right.isInstanceOf[NumericComponent])

      // increment next left numeric value
      getNumericToLeft(parents) match {
        case Some(x) => x.add(left.asInstanceOf[NumericComponent].value)
        case None =>
      }

      // increment next right numeric value
      getNumericToRight(parents) match {
        case Some(x) => x.add(right.asInstanceOf[NumericComponent].value)
        case None =>
      }

      // replace exploded pair with zero value
      if (parents.last.left == this) {
        parents.last.left = new NumericComponent(0)
      } else {
        parents.last.right = new NumericComponent(0)
      }

      true
    } else {
      // try to explode left, then right if no explosion is possible on the left
      left match {
        case pair: Pair if pair.tryExplode(parents :+ this) => true
        case _ => right match {
          case pair: Pair if pair.tryExplode(parents :+ this) => true
          case _ => false
        }
      }
    }
  }

  override def magnitude: BigInt = 3 * left.magnitude + 2 * right.magnitude

  override def copy: Component = new Pair(left.copy, right.copy)

  private def getNumericToLeft(parents: List[Pair]): Option[NumericComponent] = {
    var node: Component = this

    // traverse the parents (in depth-first order) and look for the first parent
    // for whom the current node is not it's left value
    //
    // this search is done one at a time
    val pIterator = parents.reverseIterator
    while (pIterator.hasNext) {
      val parent = pIterator.next()

      if (node == parent.left) {
        node = parent
      } else {
        var leftMostNumberCandidate = parent.left
        while (leftMostNumberCandidate.isInstanceOf[Pair]) {
          leftMostNumberCandidate = leftMostNumberCandidate.asInstanceOf[Pair].right
        }

        return Some(leftMostNumberCandidate.asInstanceOf[NumericComponent])
      }
    }
    None
  }

  private def getNumericToRight(parents: List[Pair]): Option[NumericComponent] = {
    var node: Component = this

    // traverse the parents (in depth-first order) and look for the first parent
    // for whom the current node is not it's left value
    //
    // this search is done one at a time
    val pIterator = parents.reverseIterator
    while (pIterator.hasNext) {
      val parent = pIterator.next()

      if (node == parent.right) {
        node = parent
      } else {
        var rightMostNumberCandidate = parent.right
        while (rightMostNumberCandidate.isInstanceOf[Pair]) {
          rightMostNumberCandidate = rightMostNumberCandidate.asInstanceOf[Pair].left
        }

        return Some(rightMostNumberCandidate.asInstanceOf[NumericComponent])
      }
    }
    None
  }
}