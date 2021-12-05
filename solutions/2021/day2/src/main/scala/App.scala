import scala.io.Source
import scala.util.Using

object App {
  def PlanCourse(inputFile: String, planner: CoursePlanner): Int = {

    Using(Source.fromFile(inputFile)) { source => {
      for (line <- source.getLines()) {
        planner.ApplyInstruction(line)
      }
    }
    }

    planner.depthPos() * planner.horizontalPos()
  }
}
