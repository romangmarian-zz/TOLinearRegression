import MathUtils._

case class NesterovAcceleratedGradient(learningRate: Double, epochs: Int, momentumTerm: Double) {
  private var trainingData: List[(Feature, Target)] = _

  def withData(lines: List[(Feature, Target)]): this.type = {
    trainingData = lines
    this
  }

  private def costFunction(hypothesis: Feature => Target, lines: List[(Feature, Target)]): Cost =
    lines.foldLeft(0d)((acc, line) => acc + (hypothesis(line._1) - line._2) ** 2) / 2

  def build: Hypothesis = {

    var theta: Double = 0
    var prevTheta: Double = -1
    var v: Double = 0

    def gradientFunction(theta: Double): Double = {
      val sum = trainingData.foldLeft(0d)((acc, data) => acc + (data._2 - makeHypothesis(theta)(data._1)) * data._1)
      sum / 2
    }

    while (math.abs(theta - prevTheta) > convergenceLimit) {
      prevTheta = theta
      v = momentumTerm * v - learningRate * gradientFunction(theta - momentumTerm * v)
      theta = theta - v
    }

    makeHypothesis(theta)
  }
}
