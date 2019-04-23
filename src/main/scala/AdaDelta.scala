import MathUtils._

case class AdaDelta(epochs: Int, term: Double, eps: Double) {

  private var trainingData: List[(Feature, Target)] = _

  def withData(lines: List[(Feature, Target)]): this.type = {
    trainingData = lines
    this
  }

  private def costFunction(hypothesis: Feature => Target, lines: List[(Feature, Target)]): Cost =
    lines.foldLeft(0d)((acc, line) => acc + (hypothesis(line._1) - line._2) ** 2) / 2

  def build: Hypothesis = {

    var theta: Double = 0
    var prevTheta: Double = 0
    var D: Double = 0
    var S: Double = 0

    def gradientFunction(theta: Double): Double = {
      val sum = trainingData.foldLeft(0d)((acc, data) => acc + (data._2 - makeHypothesis(theta)(data._1)) * data._1)
      sum / 2
    }

    for (i <- 0 to epochs) {
      prevTheta = theta
      val gradientVal = gradientFunction(theta)
      D = term * D + (1 - term) * ((theta - prevTheta) ** 2)
      S = term * S + (1 - term) * (gradientVal ** 2)
      theta = theta + (math.sqrt(D + eps) / math.sqrt(S + eps)) * gradientVal
    }

    makeHypothesis(theta)
  }
}