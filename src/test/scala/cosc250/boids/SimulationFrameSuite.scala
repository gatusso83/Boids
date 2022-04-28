package cosc250.boids

/**
  * SimulationFrames are immutable data classes with functions to methods to produce new immutable values.
  * They are eminently testable - although you will probably want to use a small number of boids (e.g. 2 or 3) in
  * your tests.
  */
class SimulationFrameSuite extends munit.FunSuite {

  val testBoids:SimulationFrame = SimulationFrame(Seq(Boid(Vec2(10,10),Vec2(1,1)),Boid(Vec2(20,20),Vec2(2,2)),Boid(Vec2(30,30),Vec2(2,2))))
  val boid:Boid = Boid(Vec2(0,0),Vec2(1,1))
  val testDistBoids: SimulationFrame = SimulationFrame(Seq(Boid(Vec2(10,10),Vec2(1,1)),Boid(Vec2(250,250),Vec2(2,2))))
  
  test("Double - should return a variance value for the separation value of all boids") {
    assertEquals(testBoids.flockSep, 133.333)
  }

}
