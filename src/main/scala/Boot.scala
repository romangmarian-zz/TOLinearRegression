import MathUtils._

object Boot extends App {

  val trainingList: List[(Feature, Target)] = List(
    10d -> 0d,
    20d -> 700d,
    30d -> 600d,
    40d -> 300,
    65d -> 100d
  )

  val startWeight = 0d
  val hypothesis = makeHypothesis(startWeight)

  StochasticGradientDescent( 0.0001, 1000).withData(trainingList).build(10)

}





