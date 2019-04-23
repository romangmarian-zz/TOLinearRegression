object MathUtils {

  val convergenceLimit: Double = 10 ** -4

  //math toolkit
  implicit class EnhancedDouble(thisDouble: Double) {

    import scala.math._

    def **(power: Int) = pow(thisDouble, power)
  }

  //when 0 you reach convergence
  //LMS algo
  type Feature = Double
  type Target = Double
  type Cost = Double


  //salary = weight1 * age
  type Hypothesis = Feature => Target

  //tetas are weights
  def makeHypothesis(weight1: Double): Hypothesis = weight1 * (_: Double)
}