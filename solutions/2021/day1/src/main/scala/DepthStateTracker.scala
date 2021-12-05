import scala.collection.mutable


// This solution uses a queue to track the data for
// both windows (of arbitrary size) at the same time.
//
// The concept is as follows:
//   Old Window = X(a), X(a+1), ..., X(a+_windowSize)
//   New Window = X(a+1), X(a+2), ..., X(a+_windowSize+1)
//
// And an increase occurs when:
//  SUM(New Window) > SUM(Old Window)
//
// Which can be simplified to:
//  SUM(X(a+1), X(a+2), ..., X(a+_windowSize+1)) > SUM(X(a), X(a+1), ..., X(a+_windowSize))
//
// Which reduces to:
//  X(a+_windowSize+1) > X(a)
//
// In other words, all we need to answer is:
//  Is the new reading larger than the oldest reading in the old window? If yes, then we saw
//  an increase
class DepthStateTracker(private var _windowSize: Int) {

  private val _recentObservations: mutable.Queue[Int] = mutable.Queue[Int]()

  private var _increases: Int = 0
  private var _decreases: Int = 0

  def observe(reading: Int): Unit = {

    if (_recentObservations.size == _windowSize) {
      val endOfTrailingWindow: Int = _recentObservations.dequeue()

      if (reading > endOfTrailingWindow) {
        _increases = _increases + 1
      }

      if (reading < endOfTrailingWindow) {
        _decreases = _decreases + 1
      }
    }

    _recentObservations.append(reading)
  }

  def increases(): Int = _increases

  def decreases(): Int = _decreases
}
