# Assignment 2: Boids

In a pure functional language, such as Haskell, the code is a pure (no side-effects) description of a program which is
then run in a runtime environment.

As Scala is mixed paradigm, we sometimes find ourselves "pushing mutation to the edges", so that the core parts of your code
can be pure. For instance, "effect types" (types that describe a program that has input and output effects) may have an 
`unsafeRunSync` or `unsafeRunAsync` method which is not pure. You then have a pure program in most of the code, with a 
single call to an `unsafeRun` method in the `main` method - the mutation has been pushed to the edges of the program.

We're not going to go quite that far - this is only your second assignment in the unit. However, we are going to try
to separate our functional code from our imperative code somewhat. And we're going to try to make the "important"
code functional (and more testable).

We're going to do a simulation where:

* The core logic of the simulation (the current frame, and the "frame memory" storing previously calculated frames) is
  written functionally.
* The UI and a SimulationController class are written imperatively.

The simulation is *boids*.

## Boids

Boids ("bird-oids") is a simulation of flocking introduced in a paper by
Craig Reynolds in 1987. You can see an online version of how these boids
behave on the Processing site, here:

https://processing.org/examples/flocking.html

Each boid has a position and a velocity. At each step, the boid will 
move according to its velocity

    newPosition = position + velocity

And change velocity based on a calculated acceleration

    newVelocity = velocity + acceleration
    
Note that there's no measure of how long the timestep is. 

The acceleration is generally calculated as a mix of three forces for each
boid:

* Separation - if other boids are too close, it will try to move away from them
* Alignment - it will try to match the velocity and direction of other boids within 50 pixels
* Cohesion - it will try to steer towards the middle of its neighbours (where "neighbours" is all boids within 50 pixels)

Boids have a maximum velocity, and a maximum acceleration they can apply when steering

In our assignment 2, you're going to be implementing an augmented boids
simulation using a mixture of functional and imperative programming.

## Mixing functional and imperative programming

* Everything in `FrameMemory`, `SimulationFrame`, and `Boid` is pure and functional
* `SimulationController` and the UI classes are imperative.

There are also various properties that we're going to add to our simulation:

* The wind
* The ability to "startle" the boids by applying a random impulse to
  each boid in the next frame
* The ability to tell the simulation that at the next frame, it should
  add a boid

## What's provided

I have provided most of the classes, but I've stripped out some of their
implementation.

Fully implemented, you have:

* `Vec2` -- a class that can do vector addition and multiplication. So
   that if you say `newPosition = position + velocity` it just works.
  
* `BoidsPanel` -- a panel that knows how to draw a `Seq[Boid]`

  
Partially implemented, you have: 
   
* `Boid` -- an immutable representation of a Boid. You will need to
   work functionally to produce new sequences of `Boid`

* extension methods for `Seq[Boid]` that I think will make your algorithms in `Boid` cleaner to write

* `SimulationFrame`, which holds a `Seq[Boid]` but can also report various statistics on it

* `FrameMemory`, which holds a memory of the last *n* frames of the simulation
   
* `SimulationController` -- a class the UI buttons and timer can call to control and run the simulation.

* `BoidsApp` -- the runnable program, that creates a window, and adds the
  `BoidsPanel` and some buttons. Note that the buttons' event handlers 
  have not all yet been implemented.
  
## What the components need to do

* The **Action Replay** button must take the simulation back to the oldest frame in  
  the memory buffer (typically one second) and let the simulation continue 
  again from there.  
  Hint: think about which end you want to insert the frame.
  
* The **wind** buttons should set the wind strength and direction. As the
  boids' velocity is normalised on the next tick, in practice this works as a
  fairly effective flock steering mechanism. If you set the wind blowing right, 
  you should see the flock sweep around to follow the wind.
  
* The **Startle boids** button should cause the simulation, at the next time
  step, to perturb each boid by a velocity of `startleStrength` in a random 
  direction (a different random direction for each boid)
  
* The **Regenesis** button should push a frame into the queue with `numBoids` boids heading from the
  centre in random direction. Note this shoud *not* clear the queue -- hitting
  **Action Replay** quickly after hitting **Regenesis** should jump back
  into one of the old states. 

* Clicking a point on the canvas should add a new boid into the simulation, 
  heading in a random direction with a velocity of 1

* There are also three labels at the foot of the simulation, which are intended to show some
  data about the flock. (These are drawn from `Simulation.flockDir`, `Simulation.flockSpeed`, and `Simulation.flockSep`
  which you'll need to implement.)

  - Flock direction: The average direction of the flock. (Sum the velocity vectors and take its theta). You should be
    able to see if this is working by seeing how it changes with the wind buttons

  - Flock speed: The average speed of the flock. (Take the average of the magnitude of the velocity vectors). This should
    stay more or less constant, because of how boids work

  - Flock separation: The variance of the position of the flock. This one's a little trickier. Find the centroid of the flock
    (average the position vectors). Then for each boid, calculate the square of its distance from the centroid. Return the 
    mean of that. To test this, hit the "explosion of boids" button. It should drop to nearly zero and then grow.

## Tests

There are 3 marks for writing tests of functional components in your code (2 each for `Boid`, `Seq[Boid]`, and 
`SimulationFrame` and/or `FrameMemory`). The point of this is not to ensure broad test coverage - this is not a 
test-driven development unit - but to give you a little experience in how making code functional and composable makes it 
amenable to unit testing.

## Marking

The marking is aimed to be able to be done quickly, with the written feedback being
more formative and open-ended.

Functionality: 

* The boids simulation works: 6
* Adding a boid by clicking the canvas works: 1
* Wind works: 1
* Startle works: 1
* Regenesis works: 1 
* Action replay works: 1
* Mean direction, separation, and velocity works: 1 (together)

Tests you've written:

* At least 2 tests for Boid: 1
* At least 2 tests for extension methods on Seq[Boid]: 1
* At least 2 tests for SimulationFrame and/or FrameMemory: 1

Quality: 

* Overall quality judgment (functional, readable, tidy, concise): 5
