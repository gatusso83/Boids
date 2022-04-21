package cosc250.boids

/**
  * We've kept mutation in the simulation just in this top-level object.
  */
object SimulationController {

  /** Wrap width of the simulation. ie, for any Boid, 0 <= x < 640 */
  val width = 640

  /** Wrap height of the simulation. ie, for any Boid, 0 <= y < 480 */
  val height = 480

  /** How many frames of the simulation to hold */
  val frameMemoryLength = 60

  /** How many boids to start with in the simulation */
  val numBoids = 100

  /** When the wind is blowing, how strongly it blows */
  val windStrength = 0.03

  /** The wind -- an optional acceleration vector */
  var wind:Option[Vec2] = None

  /**
    * Sets a wind blowing at windStrength, at this angle.
    * Note that a northerly wind blows **from** the north, so we multiply the vector by -1.
    */
  def setWindDirection(theta:Double):Unit = {
    wind = Option(Vec2.fromRTheta(windStrength, theta) * -1)
  }

  /** A container that can hold a boid to add on the next frame */
  var insertBoid:Option[Boid] = None
  
  /**s
    * A function that will run for the next frame only over each boid in the system,
    * producing an acceleration vector to add to a Boid
    */
  var oneTimeFunction:Option[Boid => Vec2] = None
    
  /**
    * Resets the events that should occur one time only
    */
  def resetOneTimeEvents():Unit = {
    oneTimeFunction = None
  }

  /**
    * A queue containing the last `frameMemory frames`.
    * We're using an immutable queue, but we've put it in a var, given that this controller is mutable.
    */
  var frameMemory = FrameMemory(SimulationFrame.explosionOfBoids(numBoids), frameMemoryLength)

  /** The current frame */
  def current:SimulationFrame = {
    frameMemory.currentFrame
    
  }
  
  /** Called by a click to the canvas, to say that in the next frame, a boid should be inserted */
  def pushBoid(b:Boid):Unit = {
    insertBoid = Some(b)
    val newSeq = (frameMemory.currentFrame.boids :+ insertBoid.get)
    SimulationController.pushFrame(SimulationFrame(newSeq))
  }

  /** Called by the Action Replay button to jump back in the memory buffer */  //Change type from Seq[Boid] to Unit as per Will's Response 
  def resetQueue():Unit = {
    SimulationController.pushFrame(frameMemory.oldestFrame)
  }

  /** Progress to the next frame in the simulation */
  def update():Unit = {    
    SimulationController.pushFrame(current.nextFrame(wind,oneTimeFunction))
    resetOneTimeEvents()
  }

  /** Force the simulation to use this as the next frame */
  def pushFrame(frame:SimulationFrame):Unit = {
    frameMemory = frameMemory.pushFrame(frame)
    
  }
}
