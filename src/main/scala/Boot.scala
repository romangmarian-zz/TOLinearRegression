import MathUtils._

object Boot extends App {

  val trainingList: List[(Feature, Target)] = List(
    10d -> 0d,
    20d -> 400d,
    25d -> 400d,
    30d -> 500d,
    35d -> 550d,
    40d -> 600d,
    50d -> 700d,
    55d -> 720d,
    65d -> 600d
  )

  val startWeight = 0d
  val hypothesis = makeHypothesis(startWeight)

  println("BatchGD")
  println(BatchGradientDescent( 0.0001, 1000).withData(trainingList).build(39))

  println("StochasticGD")
  println(StochasticGradientDescent( 0.0001, 1000).withData(trainingList).build(39))

  println("MomentumGD")
  println(MomentumGradientDescent( 0.0001, 1000, 0.9).withData(trainingList).build(39))

  println("NesterovAcceleratedGradient")
  println(NesterovAcceleratedGradient( 0.0001, 1000, 0.9).withData(trainingList).build(39))

  println("AdaGrad")
  println(AdaGrad( 0.1, 10000, 10 ** (-4)).withData(trainingList).build(39))

  println("AdaDelta")
  println(AdaDelta(10000, 0.95, 10 ** (-4)).withData(trainingList).build(39))
}





