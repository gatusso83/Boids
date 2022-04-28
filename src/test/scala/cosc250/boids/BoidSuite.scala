package cosc250.boids

/**
  * A place for you to write some boid tests.
  *
  * Boids are an immutable class containing functions. That makes them relatively straightforward to test --
  * except that the values within them are doubles, which are hard to compare exactly. Instead, test if they
  * are close (i.e. within a certain amount +/- what you're looking for).
  */
class BoidSuite extends munit.FunSuite {

  // A place for you to write tests. Some suggested tests to start with have been sketched below

  // Let's start with the extension methods closeTo, centroid and averageVelocity on Seq[Boid]...
  val testBoids:Seq[Boid] = Seq(Boid(Vec2(10,10),Vec2(1,1)),Boid(Vec2(20,20),Vec2(2,2)),Boid(Vec2(30,30),Vec2(3,3)))
  val boid:Boid = Boid(Vec2(0,0),Vec2(1,1))

  test("Seq[Boid] should be able to filter just those close to a certain point") {
    assertEquals(testBoids.closeTo(Vec2(0,0), 40), Seq(Boid(Vec2(10,10),Vec2(1,1)),Boid(Vec2(20,20),Vec2(2,2))))
  }

  test("Seq[Boid] should be able to calculate its centroid") {
    assertEquals(testBoids.centroid, Vec2(20,20))
  }

  test("Seq[Boid] should be able to calculate its average velocity") {
    assertEquals(testBoids.averageVelocity, Vec2(2,2))
  }

  test("Seq[boid] should be able to calculate separation acceleration") {
    assertEquals(boid.separate(testBoids), Vec2(0.05303300858899106,0.05303300858899107))
  }
  
  test("Seq[boid] should be able to calculate an align acceleration") {
    assertEquals(boid.align(testBoids), Vec2(0.02121320343559643,0.02121320343559642))
  }
}
