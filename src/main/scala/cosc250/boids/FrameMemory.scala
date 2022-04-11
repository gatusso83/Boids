package cosc250.boids

import scala.collection.immutable.Queue

/**
  * Holds the replay buffer for our simulation.
  *
  * @param queue - the queue of frames that is the memory
  * @param max - the max number of frames to hold
  */
class FrameMemory(queue:Queue[SimulationFrame], max:Int) {

  /** An alternative constructor, so we can say FrameMemory(startFrame, maxFrames) */
  def this(startFrame:SimulationFrame, max:Int) = this(Queue(startFrame), max)

  def currentFrame:SimulationFrame = {
    this.queue.last
  }
    // Remember, items join queues at the back.
  

  def oldestFrame:SimulationFrame =
    this.queue.front

  def pushFrame(frame:SimulationFrame):FrameMemory = {
    val newQ = queue.enqueue(frame)
    FrameMemory(newQ,max)    
  }
    // Don't forget to dequeue old frames if it's getting too long.
    
    
    //this.queue += frame
    //There is a some information about dequeue here: https://www.handsonscala.com/chapter-4-scala-collections.html

}
