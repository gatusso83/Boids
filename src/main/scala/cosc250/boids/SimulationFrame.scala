package cosc250.boids
import scala.math.BigDecimal

/**
  * Represents the state in our simulation.
  *
  * A simulation frame contains an immutable sequence of Boids. It has methods for reporting various measures about
  * the boids in the frame. It has a method for producing a new simulation frame
  */
case class SimulationFrame(boids:Seq[Boid]) {

  /** The current average direction of the flock. Add up all the boids' velocity vectors, and take the theta. */
  def flockDir:Double =
    BigDecimal(boids.averageVelocity.theta).setScale(3,BigDecimal.RoundingMode.HALF_UP).toDouble

  /** The current average speed of the flock. Take the mean of all the boids' velocity magnitudes. */
  def flockSpeed:Double =
    BigDecimal(boids.averageVelocity.magnitude).setScale(3,BigDecimal.RoundingMode.HALF_UP).toDouble
    

  /**
    * The variance of the flock's positions, ignoring the fact we wrap around the screen.
    * To get this one:
    *   * Calculate the centroid of the flock (Add all the position vectors, and divide by the number of boids)
    *   * Calculate the square of the distance of each boid from this centroid, and sum them.
    *      i.e., sum Math.pow((b.position - centroid).magnitude, 2)
    *   * Divide this by the number of boids.
    *
    * We'll probably eyeball the code for this one, given we're going to find it harder to eyeball whether the number
    * on the screen looks right!
    */
  def flockSep:Double = {
    val cent = boids.foldLeft(Vec2(0,0)){(acc, boidPos) => acc + boidPos.position}/boids.length
    val squareDist = boids.map(boid => Math.pow((boid.position - cent).magnitude,2))
    val res = squareDist.foldLeft(0.0){((a,b) => a + b)}/squareDist.length
    BigDecimal(res).setScale(3,BigDecimal.RoundingMode.HALF_UP).toDouble
  }
  

  /** This function should calculate the next set of boids assuming there is no wind & no one-time functions applied */
  def nextBoids:Seq[Boid] = { 
    boids.map(boid => SimulationController.wind match
      case None => (boid.update(boid.flock(boids), Vec2(0,0)))
      case Some(wind) => boid.update(boid.flock(boids), SimulationController.wind.get))
  }
      
  /**
    *
    * @param wind - a force applied to every boid. We've called it "wind" but in practice, it'll steer the flock.
    * @param oneTimeFunction - a function to apply to every boid (e.g. startle).
    * @return
    */
  def nextFrame(wind:Option[Vec2] = None, oneTimeFunction:Option[Boid => Vec2] = None):SimulationFrame =
    (wind, oneTimeFunction) match
      case (None, None) => SimulationFrame(nextBoids)
      case (Some(wind), None) => SimulationFrame(nextBoids.map(boid => Boid(boid.position, boid.velocity + wind)))
      case (None, Some(oneTimeFunction)) => SimulationFrame(nextBoids.map(boid => Boid(boid.position, boid.velocity + oneTimeFunction(boid))))  
      case (Some(wind), Some(oneTimeFunction)) => SimulationFrame(nextBoids.map(boid => Boid(boid.position, boid.velocity + wind + oneTimeFunction(boid) )))      
}

object SimulationFrame {

  /** Generates boids in the centre of the simulation, moving at v=1 in a random direction */
  def explosionOfBoids(i:Int):SimulationFrame = {
    val startPos = Vec2(SimulationController.width/2, SimulationController.height/2)
    SimulationFrame(Seq.fill(i)(Boid(startPos, Vec2.randomDir(1))))
  }  

}