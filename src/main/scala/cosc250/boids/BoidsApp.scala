package cosc250.boids

import java.awt.event.{MouseAdapter, MouseEvent, MouseListener}
import java.awt.{BorderLayout, Dimension, FlowLayout, GridLayout}
import javax.swing._

import scala.collection.mutable
import scala.util.Random

/**
 * You shouldn't edit this file
 */
object BoidsApp {

  /** The main window */
  val window = new JFrame()

  /** A panel to render our boids */
  val boidsPanel = new BoidsPanel

  /** Action replay button */
  val replay = new JButton("Action Replay")

  val nwWind = new JButton("<html>&#x2198;</html>")
  val nWind = new JButton("<html>&darr;</html>")
  val neWind = new JButton("<html>&#x2199;</html>")
  val wWind = new JButton("<html>&rarr;</html>")
  val stopWind = new JButton("<html>&times;</html>")
  val eWind = new JButton("<html>&larr;</html>")
  val swWind = new JButton("<html>&#x2197;</html>")
  val sWind = new JButton("<html>&uarr;</html>")
  val seWind = new JButton("<html>&#x2196;</html>")

  val startle = new JButton("Startle boids")

  val regenesis = new JButton("Regenesis")

  val directionLabel = new JLabel("Flock direction: xx radians")
  val velocityLabel = new JLabel("Flock speed: xx")
  val separationLabel = new JLabel("Flock separation: xx")

  @main def run = {

    val container = new JPanel()
    container.setLayout(new BorderLayout())
    container.add(boidsPanel, BorderLayout.CENTER)

    val controlsContainer = Box.createVerticalBox()

    val windPanel = new JPanel()
    windPanel.setLayout(new GridLayout(3, 3))
    windPanel.add(nwWind)
    windPanel.add(nWind)
    windPanel.add(neWind)
    windPanel.add(wWind)
    windPanel.add(stopWind)
    windPanel.add(eWind)
    windPanel.add(swWind)
    windPanel.add(sWind)
    windPanel.add(seWind)
    windPanel.setAlignmentX(0)

    val windStrength = SimulationController.windStrength

    nwWind.addActionListener((_) => SimulationController.setWindDirection(Vec2.NW))
    nWind.addActionListener((_) => SimulationController.setWindDirection(Vec2.N))
    neWind.addActionListener((_) => SimulationController.setWindDirection(Vec2.NE))
    wWind.addActionListener((_) => SimulationController.setWindDirection(Vec2.W))
    stopWind.addActionListener((_) => SimulationController.wind=None)
    eWind.addActionListener((_) => SimulationController.setWindDirection(Vec2.E))
    swWind.addActionListener((_) => SimulationController.setWindDirection(Vec2.SW))
    sWind.addActionListener((_) => SimulationController.setWindDirection(Vec2.S))
    seWind.addActionListener((_) => SimulationController.setWindDirection(Vec2.SE))

    val wc = new JLabel("Wind controls")
    wc.setAlignmentX(0)
    controlsContainer.add(wc)
    controlsContainer.add(windPanel)


    controlsContainer.add(new JLabel("Actions"))
    controlsContainer.add(replay)
    controlsContainer.add(startle)
    controlsContainer.add(regenesis)
    controlsContainer.add(new JPanel())

    container.add(controlsContainer, BorderLayout.EAST)

    val dataContainer = new JPanel()
    dataContainer.setLayout(new GridLayout(1, 3))
    dataContainer.add(directionLabel)
    dataContainer.add(velocityLabel)
    dataContainer.add(separationLabel)

    container.add(dataContainer, BorderLayout.SOUTH)


    window.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    window.add(container)
    window.setSize(container.getPreferredSize)
    window.setVisible(true)

    replay.addActionListener({ (evt) =>
      SimulationController.resetQueue()
    })

    startle.addActionListener({ (evt) =>
      SimulationController.oneTimeFunction = Some(Boid.startleFunction)
    })

    regenesis.addActionListener({ (evt) =>
      SimulationController.pushFrame(SimulationFrame.explosionOfBoids(SimulationController.numBoids))
    })

    boidsPanel.addMouseListener(new MouseAdapter {
      override def mouseClicked(e: MouseEvent):Unit = {
        println("CLicked")
        SimulationController.pushBoid(Boid(Vec2(e.getX, e.getY), Vec2.randomDir(1)))
      }
    })

    val timer = new Timer(16, (e) => {
      SimulationController.update()
      boidsPanel.setBoids(SimulationController.current.boids)
      SwingUtilities.invokeLater(() =>
        directionLabel.setText(s"Flock direction: ${SimulationController.current.flockDir} radians")
        velocityLabel.setText(s"Flock speed: ${SimulationController.current.flockSpeed} ")
        separationLabel.setText(s"Flock separation: ${SimulationController.current.flockSep}")
      )
    })
    timer.start()

  }

}
