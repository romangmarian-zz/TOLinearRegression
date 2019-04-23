import MathUtils._

case class Adam(learningRate: Double, epochs: Int, term1: Double, term2: Double, eps: Double) {

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
    var V: Double = 0
    var S: Double = 0
    var Vm: Double = 0
    var Sm: Double = 0

    def gradientFunction(theta: Double): Double = {
      val sum = trainingData.foldLeft(0d)((acc, data) => acc + (data._2 - makeHypothesis(theta)(data._1)) * data._1)
      sum / 2
    }

    for (i <- 1 to epochs) {
      val gradientVal = gradientFunction(theta)
      V = term1 * V + (1 - term1) * gradientVal
      S = term2 * S + (1 - term2) * (gradientVal ** 2)
      Vm = V / (1 - term1 ** i)
      Sm = S / (1 - term2 ** i)
      theta = theta + (learningRate / (math.sqrt(Sm) + eps)) * Vm
    }

    makeHypothesis(theta)
  }
}