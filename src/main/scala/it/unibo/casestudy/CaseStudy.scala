package it.unibo.casestudy

import java.util.Optional

import it.unibo.alchemist.model.implementations.molecules.SimpleMolecule
import it.unibo.alchemist.model.interfaces.{Layer, Position}
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

import scala.util.{Success, Try}

case class Logs(id: ID)(val content: String, val timestamp: Long) {
  override def toString: String = super.toString + s"[$timestamp]"
}

class CaseStudy extends AggregateProgram with StandardSensors with ScafiAlchemistSupport with EdgeFields with EdgeFieldsLib {
  val SENSOR_DETECTOR_ID = "detectorId"
  val SENSOR_COLLECTOR_ID = "collectorId"
  val SENSOR_SURVEILLANCE_AREA_SIZE = "surveillanceAreaSize"
  val SENSOR_COMMUNICATION_RADIUS = "commRadius"
  val SENSOR_WARNING_THRESHOLD = "warningThreshold"
  val SENSOR_LOCAL_WARNING = "localWarning" // from a layer

  override def main(): Any = {
    /*
    val g = gradient(mid==0, nbrRangeEF)
    node.put("g", g)
    node.put("src", mid==0)
    node.put("dest", mid==100)
    val c = channel(mid==0, mid==100, 0.1 /* node.get[Double]("commRadius")*1.5 */)
    node.put("c", c)
    g
     */
    def isDetector: Boolean = mid == sense[ID](SENSOR_DETECTOR_ID)
    def isCollector: Boolean = mid == sense[ID](SENSOR_COLLECTOR_ID)
    lazy val surveillanceAreaSize: Double = sense[Double](SENSOR_SURVEILLANCE_AREA_SIZE)
    lazy val communicationRadius: Double = sense[Double](SENSOR_COMMUNICATION_RADIUS)
    lazy val warningThreshold: Double = sense[Double](SENSOR_WARNING_THRESHOLD)
    def localWarning: Double = senseEnv[Double](SENSOR_LOCAL_WARNING)
    val warningRetentionTime = 10
    val channelWidth = 10

    val surveillanceArea = gradient(isDetector, nbrRangeEF)
    val inSurveillanceArea: Boolean = surveillanceArea < surveillanceAreaSize
    val meanWarning: Double = branch(inSurveillanceArea){ Cwmp(isDetector, communicationRadius, localWarning, 0.0) } { 0.0 }
    val warningDetected = meanWarning > warningThreshold
    val logs: Set[Logs] = branch(inSurveillanceArea) {
      val warning: Boolean = broadcast(surveillanceArea, keep(warningDetected, warningRetentionTime))
      node.put("warning", warning)
      branch(warning) {
        Cwmpg[Set[Logs]](isDetector, communicationRadius, if (localWarning > 0) Set(Logs(mid)("", timestamp())) else Set.empty, Set.empty,
          accumulate = (ef, t) => t, extract = (v, w, th, Null) => v)
      }{ Set.empty }
    } { Set.empty }
    val reportingChannel = channel(isDetector, isCollector, channelWidth)
    val reportedLogs: Set[Logs] = branch(reportingChannel){ broadcast(surveillanceArea, logs) }{ Set.empty }

    node.put("meanWarning", meanWarning)
    node.put("localWarning", localWarning)
    node.put("src", isDetector)
    node.put("dest", isCollector)
    node.put("area", inSurveillanceArea)
    node.put("warningDetected", warningDetected)
    node.put("c", reportingChannel)
    reportedLogs
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