package it.unibo.casestudy

import java.util.Optional

import it.unibo.alchemist.model.implementations.molecules.SimpleMolecule
import it.unibo.alchemist.model.implementations.positions.LatLongPosition
import it.unibo.alchemist.model.interfaces.{Layer, Position}
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.scafi.space.Point2D

import scala.util.{Success, Try}

case class Logs(id: ID)(val content: String, val timestamp: Long) {
  override def toString: String = s"Logs($id)[$timestamp]"
}

case class WarningReport(from: ID, sumWarning: Double, numDevices: Double, timestamp: Long, logs: Set[Logs]) {
  def meanWarning = sumWarning / numDevices
}

class CaseStudy extends AggregateProgram with StandardSensors with ScafiAlchemistSupport with XCLangImpl with XCLib {
  val SENSOR_DETECTOR_ID = "detectorId"
  val SENSOR_COLLECTOR_ID = "collectorId"
  val SENSOR_SURVEILLANCE_AREA_SIZE = "surveillanceAreaSize"
  val SENSOR_COMMUNICATION_RADIUS = "commRadius"
  val SENSOR_WARNING_THRESHOLD = "warningThreshold"
  val LAYER_LOCAL_WARNING = "localWarning" // from a layer
  val SENSOR_EVENT_WINDOW = "eventWindow"
  val SENSOR_OBSTACLE_WINDOW = "obstacleWindow"
  val SENSOR_WARNING_RETENTION_TIME: String = "warningRetentionTime"
  val SENSOR_LOCAL_NUM_OF_DEVICES: String = "totalNumOfDevices"
  val SENSOR_IS_COLLECTOR: String = "collector"
  val SENSOR_IS_DETECTOR: String = "detector"
  val LAYER_OBSTACLE_LOCATION: String = "obstacle"

  override def main(): Any = {
    cleanState

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
      obstacle > 0.1
    } else false
    lazy val warningRetentionTime = sense[Int](SENSOR_WARNING_RETENTION_TIME)
    val channelWidth = 10

    val surveillanceArea = gradient(isDetector, nbrRangeEF)
    val inSurveillanceArea: Boolean = surveillanceArea < surveillanceAreaSize
    val (sumWarning, numDevices) = branch(inSurveillanceArea){
      val (sumWarning, numDevices) = Cwmpg[(Double,Double)](isDetector, communicationRadius, (localWarning, 1.0), (0.0, 0.0),
        accumulate = (v, l) => (v._1.foldSum(l._1), v._2.foldSum(l._2)),
        extract = (v, w, th, Null) => pair(v._1 * w, v._2 * w)
      )
      node.put("sumWarningAtDetector", if(isDetector) sumWarning else 0)
      node.put("meanWarningOverallAtDetector", if(isDetector) sumWarning / totalNumOfDevices else 0)
      node.put("numDevicesAtDetector", if(isDetector) numDevices else 0)
      (sumWarning, numDevices)
    } {
      node.put("sumWarningAtDetector", 0)
      node.put("numDevicesAtDetector", 0)
      (0.0, 0.0)
    }
    val meanWarning = sumWarning / numDevices
    val warningDetected = meanWarning > warningThreshold
    val logs: Set[Logs] = branch(inSurveillanceArea) {
      val warning: Boolean = broadcast(surveillanceArea, keep(warningDetected, warningRetentionTime, (_: Boolean) == true))
      node.put("warning", warning)
      branch(warning) {
        Cwmpg[Set[Logs]](isDetector, communicationRadius, if (localWarning > 0) Set(Logs(mid)("", timestamp())) else Set.empty, Set.empty,
          accumulate = (ef, t) => t ++ ef.fold(Set.empty)(_++_), extract = (v, w, th, Null) => v)
      }{ Set.empty }
    } { node.put("warning", false); Set.empty }

    val dataToBeReported = WarningReport(mid, sumWarning, numDevices, timestamp(), logs)
    val reportingChannel = branch(!isObstacle){ channel(isDetector, isCollector, channelWidth) } { false }
    val reportingData: Option[WarningReport] = branch(reportingChannel){ Option(broadcast(surveillanceArea, dataToBeReported)) }{ Option.empty }

    node.put("isObstacle", isObstacle)
    node.put("logs", logs)
    node.put("meanWarning", meanWarning)
    node.put("meanWarningAtDetector", if(isDetector) meanWarning else 0.0)
    node.put("meanWarningAtCollector", reportingData.map(_.meanWarning).filter(_ => isCollector).getOrElse(0.0))
    node.put("sumWarningAtCollector", reportingData.map(_.sumWarning).filter(_ => isCollector).getOrElse(0.0))
    node.put("surveillanceAreaGradient", surveillanceArea)
    node.put("inSurveillanceArea", if(inSurveillanceArea) 1 else 0)
    node.put("localWarningInSystem", localWarning)
    node.put("localWarningInSurveillanceArea", if(inSurveillanceArea) localWarning else Double.NaN)
    node.put("src", isDetector)
    node.put("dest", isCollector)
    node.put("area", inSurveillanceArea)
    node.put("warningDetected", warningDetected)
    node.put("c", reportingChannel)
    node.put("dataAtCollector", reportingData.map(_.logs.size.toDouble).filter(_ => isCollector).getOrElse(0.0))
    val p = currentPos()
    val newPos =  new LatLongPosition(p.getLatitude + (nextRandom() - 0.5), p.getLongitude + (nextRandom() - 0.5))
    node.put("move_to", newPos)
    if(!isDetector && !isCollector) {
    }
    reportingData
  }

  def currentPos(): LatLongPosition = sense[LatLongPosition](LSNS_POSITION)

  def cleanState() = {
    node.put("meanWarningAtCollector", 0.0)
  }

  implicit def OptionalToOption[E](p : Optional[E]) : Option[E] = if (p.isPresent) Some(p.get()) else None

  private def findInLayers[A](name : String) : Option[A] = {
    val layer : Option[Layer[Any, Position[_]]] = alchemistEnvironment.getLayer(new SimpleMolecule(name))
    val node = alchemistEnvironment.getNodeByID(mid())
    layer.map(l => l.getValue(alchemistEnvironment.getPosition(node)))
      .map(value => Try(value.asInstanceOf[A]))
      .collect { case Success(value) => value }
  }

  //TODO fix in the alchemist
  def senseEnv[A](name: String): A = {
    findInLayers[A](name).get
  }
}