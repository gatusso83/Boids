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
    println("Warning, you haven't implemented flockDir!")
    0d

  /** The current average speed of the flock. Take the mean of all the boids' velocity magnitudes. */
  def flockSpeed:Double =
    println("Warning, you haven't implemented flockSpeed!")
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
    println("Warning, you haven't implemented flockSep!")
    0d

  /** This function should calculate the next set of boids assuming there is no wind & no one-time functions applied */
  def nextBoids:Seq[Boid] = {

  // OMG FYI the reason I am so confused as we are looking at a class SimulationFrame, so we are building all the methods into it and therefore anywhere where there is
  //a SimulationFrame(boids)... boids is used as the list of Boids throughout the methods here. 
    
    //boids.map(b => ((b._1.x + b._2.x, b._1.y + b._2.y),(b._2.x,b._2.y)))
    //boids.map(a => ((a.position.x +(a.velocity.x), a.position.y +(a.velocity.y)),(a.velocity.x, a.velocity.y))):Seq[Boid]
    val newb = boids.map(b => Boid(b.position + b.velocity, b.velocity))
    println(newb)
    newb

   // boids.map((b:Boid => (b._1._1 +(b._2._1), b._1._2 + b._2._2,b._2._1,b._2._2))
   // boids.map((b:Boid => Boid(b._1 +b._2, b._1._2 + b._2._2,b._2._1,b._2._2))

    //boids.map((p,v) => (p._1 + p._2, p._1 + p._1),(v._1, v._2)
    //boids.map((p, v) => ((p._1 + v._1, p._2 + v._2),v))
    //https://stackoverflow.com/questions/24105479/scala-case-class-update-value
    //boids.foreach(((p1, p1), (v1,v2)) => ((p1+v1), (p2+v2)))
  
    //boids.map(Boid(+(_2), Vec2.velocity))

    /// have a look at map(_ * 2)
    //SimulationFrame.currentFrame.map((position._1 + velocity._1, position._2 + velocity._2),velocity) 
  }
    // 

  /**
    *
    * @param wind - a force applied to every boid. We've called it "wind" but in practice, it'll steer the flock.
    * @param oneTimeFunction - a function to apply to every boid (e.g. startle).
    * @return
    */
  def nextFrame(wind:Option[Vec2] = None, oneTimeFunction:Option[Boid => Vec2] = None):SimulationFrame =
    val newBoids = SimulationFrame(nextBoids)
    newBoids
}

object SimulationFrame {

  /** Generates boids in the centre of the simulation, moving at v=1 in a random direction */
  def explosionOfBoids(i:Int):SimulationFrame = {
    val startPos = Vec2(SimulationController.width/2, SimulationController.height/2)
    println (SimulationFrame(Seq.fill(i)(Boid(startPos, Vec2.randomDir(1)))))
    val boidss = (Seq.fill(i)(Boid(startPos, Vec2.randomDir(1))))
    SimulationFrame(boidss)
    //val currentFrame = SimulationFrame(Seq.fill(i)(Boid(startPos, Vec2.randomDir(1))))
    //currentFrame
  }  


}