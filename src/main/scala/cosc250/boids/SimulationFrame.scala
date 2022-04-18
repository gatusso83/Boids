package cosc250.boids

/**
  * Represents the state in our simulation.
  *
  * A simulation frame contains an immutable sequence of Boids. It has methods for reporting various measures about
  * the boids in the frame. It has a method for producing a new simulation frame
  */
case class SimulationFrame(boids:Seq[Boid]) {

  /** The current average direction of the flock. Add up all the boids' velocity vectors, and take the theta. */
  def flockDir:Double =
    //println("Warning, you haven't implemented flockDir!")
    0d

  /** The current average speed of the flock. Take the mean of all the boids' velocity magnitudes. */
  def flockSpeed:Double =
    //println("Warning, you haven't implemented flockSpeed!")
    0d

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
  def flockSep:Double =
    //println("Warning, you haven't implemented flockSep!")
    0d

  /** This function should calculate the next set of boids assuming there is no wind & no one-time functions applied */
  def nextBoids:Seq[Boid] = {
     boids.map(boid => 
       val updatedPos= Vec2(boid.wrapX(boid.position.x + boid.velocity.x), boid.wrapY(boid.position.y + boid.velocity.y)) // Updated position with boundary wrapper
        Boid(updatedPos,boid.velocity))
       //println("do I get here")
       //println(boid.update(boid.flock(boids), SimulationController.wind.g))
       //boid.update(boid.flock(boids), SimulationController.wind))
  }

  /**
    *
    * @param wind - a force applied to every boid. We've called it "wind" but in practice, it'll steer the flock.
    * @param oneTimeFunction - a function to apply to every boid (e.g. startle).
    * @return
    */
  def nextFrame(wind:Option[Vec2] = None, oneTimeFunction:Option[Boid => Vec2] = None):SimulationFrame =
    (wind, oneTimeFunction) match
      case (Some(wind), None) => SimulationFrame(nextBoids.map(boid => 
                if (boid.velocity.magnitude + wind.magnitude > Boid.maxSpeed + wind.magnitude ) { 
                  Boid((boid.position + boid.velocity), boid.velocity)
                }
                else {
                  Boid((boid.position + boid.velocity),boid.velocity + wind  )
                }
      ))
                //Boid((boid.position + boid.velocity), boidVel)))
                //val windAndVel = boid.velocity + wind
                //Boid((boid.position + boid.velocity),boid.velocity + wind  )))
      case (None, None) => SimulationFrame(nextBoids.map(boid =>
        println(boid.update(boid.flock(boids), Vec2(1,1))) //SimulationFrame(nextBoids)
                boid.update(boid.flock(boids), Vec2(1,1))))
                
    
    /*wind match {
      case Some(wind) => 
        println("WIND: " + wind)
        
          SimulationFrame(nextBoids.map(boid => Boid((boid.position + boid.velocity),boid.velocity + wind  )))
          println(SimulationFrame(nextBoids.map(boid => Boid((boid.position + boid.velocity),wind + boid.velocity)))) // Not working as intended
          
      case None => SimulationFrame(nextBoids.map(boid => Boid((boid.position + boid.velocity), boid.velocity)))
      println("None: "+SimulationFrame(nextBoids.map(boid => Boid((boid.position + boid.velocity), boid.velocity))))
    }
    oneTimeFunction match {
      case None => SimulationFrame(nextBoids)
    }*/
  
}

object SimulationFrame {

  /** Generates boids in the centre of the simulation, moving at v=1 in a random direction */
  def explosionOfBoids(i:Int):SimulationFrame = {
    val startPos = Vec2(SimulationController.width/2, SimulationController.height/2)
    println(SimulationFrame(Seq.fill(i)(Boid(startPos, Vec2.randomDir(1)))))
    SimulationFrame(Seq.fill(i)(Boid(startPos, Vec2.randomDir(1))))
  }  


}