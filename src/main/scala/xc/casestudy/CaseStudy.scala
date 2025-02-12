package xc.casestudy

import java.util.Optional

import it.unibo.alchemist.model.implementations.molecules.SimpleMolecule
import it.unibo.alchemist.model.implementations.positions.LatLongPosition
import it.unibo.alchemist.model.interfaces.{Layer, Position}
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import xc._

import scala.util.{Success, Try}

/**
 * This is the XC program played by all the nodes of the system.
 * It is executed "repeatedly" in asynchronous rounds.
 * In each round, the most up-to-date information from sensors and neighbours is used.
 * Method `main` defines the main expression (i.e. the entry point of the program).
 * The program logic is intertwined with simulation-specific code for extracting parameters and exporting data.
 */
class CaseStudy extends XCProgram with XCLib {
  import CaseStudy._

  /**
   * Entry point of the program. This is the XC program played "in a loop" by all the devices in the system.
   */
  override def main(): Any = {
    // ************** Preparation ***************
    // Simulation-specific code: cleaning exports
    cleanState

    // Simulation-specific code: getting inputs, parameters, etc.
    lazy val totalNumOfDevices = sense[Double](SENSOR_LOCAL_NUM_OF_DEVICES).toInt
    def isDetector: Boolean = sense[Boolean](SENSOR_IS_DETECTOR) //mid == sense[ID](SENSOR_DETECTOR_ID)
    def isCollector: Boolean = sense[Boolean](SENSOR_IS_COLLECTOR) /*{
      var collectors = sense[List[ID]](SENSOR_COLLECTOR_ID)
      if(timestamp()>100) collectors = collectors.tail
      collectors.contains(mid)
    }*/
    lazy val surveillanceAreaSize: Double = sense[Double](SENSOR_SURVEILLANCE_AREA_SIZE)
    lazy val communicationRadius: Double = sense[Double](SENSOR_COMMUNICATION_RADIUS)
    lazy val warningThreshold: Double = sense[Double](SENSOR_WARNING_THRESHOLD)
    lazy val eventWindow: (Long, Long) = {
      val w = sense[(Int, Int)](SENSOR_EVENT_WINDOW)
      (w._1.toLong, w._2.toLong)
    }
    lazy val eventStartTime: Long = eventWindow._1
    lazy val eventEndTime: Long = eventWindow._2
    lazy val obstacleWindow: (Long, Long) = {
      val w = sense[(Int, Int)](SENSOR_OBSTACLE_WINDOW)
      (w._1.toLong, w._2.toLong)
    }
    lazy val obstacleStartTime: Long = obstacleWindow._1
    lazy val obstacleEndTime: Long = obstacleWindow._2
    def localWarning: Double = if(timestamp() >= eventStartTime && timestamp() <= eventEndTime) { senseEnv[Double](LAYER_LOCAL_WARNING) } else 0.0
    def isObstacle: Boolean = if(timestamp() >= obstacleStartTime && timestamp() <= obstacleEndTime) {
      val obstacle = senseEnv[Double](LAYER_OBSTACLE_LOCATION)
      obstacle > PARAMETER_OBSTACLE_PERCEPTION_THRESHOLD
    } else false
    lazy val warningRetentionTime = sense[Int](SENSOR_WARNING_RETENTION_TIME)
    val channelWidth = PARAMETER_CHANNEL_WIDTH

    // ************** Program logic ***************
    // Build a gradient of distances from the detector, used to define an "area" to be monitored
    val surveillanceArea = gradient(isDetector, senseDist)
    // The nodes belonging to the area are those for which the gradient is up to a threshold
    val inSurveillanceArea: Boolean = surveillanceArea < surveillanceAreaSize
    // Define a different logic of warning collection for nodes inside and outside the surveillance area
    val (sumWarning, numDevices): (Double, Double) = xcbranch(inSurveillanceArea){
      // Collect into the detector the sum of warning and the number of devices in the area
      val (sumWarning, numDevices) = collect[(Double,Double)](isDetector, communicationRadius, (localWarning, 1.0), (0.0, 1.0),
        accumulate = (v, l) => (v._1.foldSum(l._1), v._2.foldSum(l._2)),
        extract = (v, w, th, Null) => pair(v._1 * w, v._2 * w)
      )
      node.put(EXPORT_SUM_WARNING_AT_DETECTOR, if(isDetector) sumWarning else 0)
      node.put(EXPORT_MEAN_WARNING_AT_DETECTOR, if(isDetector) sumWarning / totalNumOfDevices else 0)
      node.put(EXPORT_NUM_DEVICES_AT_DETECTOR, if(isDetector) numDevices else 0)
      (sumWarning, numDevices)
    } {
      node.put(EXPORT_SUM_WARNING_AT_DETECTOR, 0)
      node.put(EXPORT_NUM_DEVICES_AT_DETECTOR, 0)
      (0.0, 1.0)
    }
    // The mean warning can be computed by just dividing the sum of warning by the number of devices
    val meanWarning = sumWarning / numDevices
    // There is a "warning situation" if the mean warning exceeds a given threshold
    val warningDetected = meanWarning > warningThreshold
    // Define a different logic of logs collection for nodes inside and outside the surveillance area
    val logs: Set[Logs] = xcbranch(inSurveillanceArea) {
      // Diffuse the warning situation from the detector to all the devices in its area
      val warning: Boolean = broadcast(surveillanceArea, keep(warningDetected, warningRetentionTime, (_: Boolean) == true))
      node.put(EXPORT_WARNING, warning)
      // Only the nodes that perceive the warning situation are involved in the process collecting logs towards the detector
      // Only the nodes for which the local warning is greater than 0 expose their logs.
      xcbranch(warning) {
        // Logs are accumulated by set union (where each log is identified by the ID of its source)
        collect[Set[Logs]](isDetector, communicationRadius, if (localWarning > 0) Set(Logs(mid)("", timestamp())) else Set.empty, Set.empty,
          accumulate = (ef, t) => t ++ ef.fold(Set.empty)(_++_), extract = (v, w, th, Null) => v)
      }{ Set.empty[Logs] }
    } { node.put(EXPORT_WARNING, false); Set.empty[Logs] }


    val dataToBeReported = WarningReport(mid, sumWarning, numDevices, timestamp(), logs)
    // A channel is built, avoiding obstacles (failed devices), from the detector to the collector (operations centre)
    val reportingChannel: Boolean = xcbranch(!isObstacle){ channel(isDetector, isCollector, channelWidth) } { false }
    // A broadcast is performed within the reporting channel, using the data at the source of the surveillanceArea gradient (i.e., from the detector)
    val reportingData: Option[WarningReport] = xcbranch(reportingChannel){ Option(broadcast(surveillanceArea, dataToBeReported)) }{ Option.empty[WarningReport] }

    // *************** Export of data for simulation results gathering + actuation (random movement) **************
    node.put(EXPORT_IS_OBSTACLE, isObstacle)
    node.put(EXPORT_LOGS, logs)
    node.put(EXPORT_MEAN_WARNING, meanWarning)
    node.put(EXPORT_MEAN_WARNING_AT_DETECTOR, if(isDetector) meanWarning else 0.0)
    node.put(EXPORT_MEAN_WARNING_AT_COLLECTOR, reportingData.map(_.meanWarning).filter(_ => isCollector).getOrElse(0.0))
    node.put(EXPORT_SUM_WARNING_AT_COLLECTOR, reportingData.map(_.sumWarning).filter(_ => isCollector).getOrElse(0.0))
    node.put(EXPORT_SURVEILLANCE_AREA_GRADIENT, surveillanceArea)
    node.put(EXPORT_INSIDE_SURVEILLANCE_AREA_AS_INT, if(inSurveillanceArea) 1 else 0)
    node.put(EXPORT_LOCAL_WARNING, localWarning)
    node.put(EXPORT_LOCAL_WARNING_IN_AREA_ONLY, if(inSurveillanceArea) localWarning else Double.NaN)
    node.put(EXPORT_SOURCE, isDetector)
    node.put(EXPORT_DESTINATION, isCollector)
    node.put(EXPORT_INSIDE_SURVEILLANCE_AREA, inSurveillanceArea)
    node.put(EXPORT_WARNING_DETECTED, warningDetected)
    node.put(EXPORT_IN_CHANNEL, reportingChannel)
    node.put(EXPORT_DATA_AT_COLLECTOR, reportingData.map(_.logs.size.toDouble).filter(_ => isCollector).getOrElse(0.0))
    val p = currentPos()
    val newPos =  new LatLongPosition(p.getLatitude + (nextRandom() - 0.5), p.getLongitude + (nextRandom() - 0.5))
    node.put(ACTUATOR_MOVE_TO, newPos)
    reportingData
  }

  def currentPos(): LatLongPosition = sense[LatLongPosition](LSNS_POSITION)

  def cleanState() = {
    node.put(EXPORT_MEAN_WARNING_AT_COLLECTOR, 0.0)
  }

  implicit def OptionalToOption[E](p : Optional[E]) : Option[E] = if (p.isPresent) Some(p.get()) else None

  private def findInLayers[A](name : String) : Option[A] = {
    val layer : Option[Layer[Any, Position[_]]] = alchemistEnvironment.getLayer(new SimpleMolecule(name))
    val node = alchemistEnvironment.getNodeByID(mid())
    layer.map(l => l.getValue(alchemistEnvironment.getPosition(node)))
      .map(value => Try(value.asInstanceOf[A]))
      .collect { case Success(value) => value }
  }

  // TODO fix in the alchemist
  def senseEnv[A](name: String): A = {
    findInLayers[A](name).get
  }
}

/**
 * Companion object for the case study program.
 * It defines data structures and parameters used by the case study program.
 */
object CaseStudy {
  case class Logs(id: ID)(val content: String, val timestamp: Long) {
    override def toString: String = s"Logs($id)[$timestamp]"
  }

  case class WarningReport(from: ID, sumWarning: Double, numDevices: Double, timestamp: Long, logs: Set[Logs]) {
    def meanWarning = sumWarning / numDevices
  }

  val SENSOR_DETECTOR_ID = "detectorId"
  val SENSOR_COLLECTOR_ID = "collectorId"
  val SENSOR_SURVEILLANCE_AREA_SIZE = "surveillanceAreaSize"
  val SENSOR_COMMUNICATION_RADIUS = "commRadius"
  val SENSOR_WARNING_THRESHOLD = "warningThreshold"
  val SENSOR_EVENT_WINDOW = "eventWindow"
  val SENSOR_OBSTACLE_WINDOW = "obstacleWindow"
  val SENSOR_WARNING_RETENTION_TIME = "warningRetentionTime"
  val SENSOR_LOCAL_NUM_OF_DEVICES = "totalNumOfDevices"
  val SENSOR_IS_COLLECTOR = "collector"
  val SENSOR_IS_DETECTOR = "detector"

  val ACTUATOR_MOVE_TO = "move_to"

  val LAYER_OBSTACLE_LOCATION = "obstacle"
  val LAYER_LOCAL_WARNING = "localWarning" // from a layer

  val EXPORT_MEAN_WARNING = "meanWarning"
  val EXPORT_IN_CHANNEL = "c"
  val EXPORT_MEAN_WARNING_AT_COLLECTOR = "meanWarningAtCollector"
  val EXPORT_SUM_WARNING_AT_DETECTOR = "sumWarningAtDetector"
  val EXPORT_MEAN_WARNING_AT_DETECTOR = "meanWarningOverallAtDetector"
  val EXPORT_NUM_DEVICES_AT_DETECTOR = "numDevicesAtDetector"
  val EXPORT_WARNING = "warning"
  val EXPORT_IS_OBSTACLE = "isObstacle"
  val EXPORT_LOGS = "logs"
  val EXPORT_SUM_WARNING_AT_COLLECTOR = "sumWarningAtCollector"
  val EXPORT_SURVEILLANCE_AREA_GRADIENT = "surveillanceAreaGradient"
  val EXPORT_INSIDE_SURVEILLANCE_AREA_AS_INT = "inSurveillanceArea"
  val EXPORT_LOCAL_WARNING = "localWarningInSystem"
  val EXPORT_LOCAL_WARNING_IN_AREA_ONLY = "localWarningInSurveillanceArea"
  val EXPORT_SOURCE = "src"
  val EXPORT_DESTINATION = "dest"
  val EXPORT_INSIDE_SURVEILLANCE_AREA = "area"
  val EXPORT_WARNING_DETECTED = "warningDetected"
  val EXPORT_DATA_AT_COLLECTOR = "dataAtCollector"

  val PARAMETER_CHANNEL_WIDTH = 10
  val PARAMETER_OBSTACLE_PERCEPTION_THRESHOLD = 0.1
}