import MathUtils._

case class StochasticGradientDescent(learningRate: Double, target: Int) {

  private var trainingData: List[(Feature, Target)] = _

  def withData(lines: List[(Feature, Target)]): this.type = {
    trainingData = lines
    this
  }

  private def costFunction(hypothesis: Feature => Target, lines: List[(Feature, Target)]): Cost =
    lines.foldLeft(0d)((acc, line) => acc + (hypothesis(line._1) - line._2) ** 2) / 2
  def build: Hypothesis = {

    var teta: Double = 0 //age 0

    for(_ <- 0 to target) {
      teta = teta + learningRate * trainingData.foldLeft(0d)((acc, data) => acc + ((data._2 - makeHypothesis(teta)(data._1)) * data._1))
    }

    makeHypothesis(teta)
  }

}
