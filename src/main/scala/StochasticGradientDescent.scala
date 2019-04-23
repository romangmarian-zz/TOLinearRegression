import MathUtils._

case class StochasticGradientDescent(learningRate: Double, epochs: Int) {
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

    while (math.abs(theta - prevTheta) > convergenceLimit) {
      prevTheta = theta
      for ((x, y) <- trainingData)
        theta = theta + learningRate * (y - makeHypothesis(theta)(x)) * x
    }

    makeHypothesis(theta)
  }
}
