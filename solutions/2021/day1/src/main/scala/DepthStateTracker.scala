class DepthStateTracker(private var _windowSize: Int) {

  // 2D array that looks like:
  //  {
  //    { <window_a reading_1>, ..., <window_a reading_n> },
  //    { <window_b reading_1>, ..., <window_b reading_n> },
  //  }
  private var _observations: Array[Array[Option[Int]]] = Array.fill(_windowSize){
    Array.fill(_windowSize){None}
  }

  // tracks which window is "current" in _lastObservations
  private var _windowIndex: Int = 0

  // tracks which reading is "current in _observations[_readingIndex]
  private var _readingIndex: Int = 0

  private var _increases: Int = 0
  private var _decreases: Int = 0

  def observe(reading: Int): Unit = {
    _observations(_windowIndex)(_readingIndex) = Some(reading)

    // move to the next index in the current window
    incrementReading()

    // move to the next window
    if (_readingIndex == 0) {
      computeDelta()
      incrementWindow()
    }

    println(s"reading --> ${_readingIndex}")
    println(s"window -->: ${_windowIndex}")
  }

  // roll reading index
  private def incrementReading(): Unit = {
    _readingIndex = (_readingIndex + 1) % _windowSize
  }

  // roll window index and "erase" the oldest window
  private def incrementWindow(): Unit = {
    _windowIndex = (_windowIndex + 1) % _windowSize
    _observations(_windowIndex) = Array.fill(_windowSize){None}
  }

  // compute if there should be an increase or decrease counted
  private def computeDelta(): Unit = {
    oldest
  }

  def increases(): Int = _increases

  def decreases(): Int = _decreases
}
