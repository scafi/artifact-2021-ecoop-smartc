package it.unibo.casestudy

import java.util.Optional

import it.unibo.alchemist.model.implementations.molecules.SimpleMolecule
import it.unibo.alchemist.model.interfaces.{Layer, Position}
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

import scala.util.{Success, Try}

case class Logs(id: ID)(val content: String, val timestamp: Long) {
  override def toString: String = s"Logs($id)[$timestamp]"
}

case class WarningReport(meanWarning: Double, logs: Set[Logs])

class CaseStudy extends AggregateProgram with StandardSensors with ScafiAlchemistSupport with EdgeFields with EdgeFieldsLib {
  val SENSOR_DETECTOR_ID = "detectorId"
  val SENSOR_COLLECTOR_ID = "collectorId"
  val SENSOR_SURVEILLANCE_AREA_SIZE = "surveillanceAreaSize"
  val SENSOR_COMMUNICATION_RADIUS = "commRadius"
  val SENSOR_WARNING_THRESHOLD = "warningThreshold"
  val SENSOR_LOCAL_WARNING = "localWarning" // from a layer
  val SENSOR_EVENT_WINDOW = "eventWindow"
  val SENSOR_WARNING_RETENTION_TIME: String = "warningRetentionTime"
  val SENSOR_LOCAL_NUM_OF_DEVICES: String = "totalNumOfDevices"

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
    lazy val totalNumOfDevices = sense[Int](SENSOR_LOCAL_NUM_OF_DEVICES)
    def isDetector: Boolean = mid == sense[ID](SENSOR_DETECTOR_ID)
    def isCollector: Boolean = mid == sense[ID](SENSOR_COLLECTOR_ID)
    lazy val surveillanceAreaSize: Double = sense[Double](SENSOR_SURVEILLANCE_AREA_SIZE)
    lazy val communicationRadius: Double = sense[Double](SENSOR_COMMUNICATION_RADIUS)
    lazy val warningThreshold: Double = sense[Double](SENSOR_WARNING_THRESHOLD)
    lazy val eventWindow: (Long, Long) = {
      val w = sense[(Int, Int)](SENSOR_EVENT_WINDOW)
      (w._1.toLong, w._2.toLong)
    }
    lazy val eventStartTime: Long = eventWindow._1
    lazy val eventEndTime: Long = eventWindow._2
    def localWarning: Double = if(timestamp() >= eventStartTime && timestamp() <= eventEndTime) { senseEnv[Double](SENSOR_LOCAL_WARNING) } else 0.0
    lazy val warningRetentionTime = sense[Int](SENSOR_WARNING_RETENTION_TIME)
    val channelWidth = 10

    val surveillanceArea = gradient(isDetector, nbrRangeEF)
    val inSurveillanceArea: Boolean = surveillanceArea < surveillanceAreaSize
    val meanWarning: Double = branch(inSurveillanceArea){
      val (sumWarning, numDevices) = Cwmpg[(Double,Double)](isDetector, communicationRadius, (localWarning, 1.0), (0.0, 0.0),
        accumulate = (v, l) => ({
          // println(s"$mid ($surveillanceArea) :: accumulating localWarning ${v._1} (local: ${l._1}")
          v._1.foldSum(l._1)
        }, v._2.foldSum(l._2)),
        extract = (v, w, th, Null) => pair(v._1 * w, v._2 * w)
      )
      node.put("warningCalculation", s"${sumWarning} / ${numDevices} = ${sumWarning/numDevices} --- ${sumWarning} / ${totalNumOfDevices} = ${sumWarning/totalNumOfDevices}")
      node.put("sumWarningAtDetector", if(isDetector) sumWarning else 0)
      node.put("meanWarningOverallAtDetector", if(isDetector) sumWarning / totalNumOfDevices else 0)
      node.put("numDevicesAtDetector", if(isDetector) numDevices else 0)
      sumWarning / numDevices.round
    } {
      node.put("sumWarningAtDetector", 0)
      node.put("numDevicesAtDetector", 0)
      0.0
    }
    val warningDetected = meanWarning > warningThreshold
    val logs: Set[Logs] = branch(inSurveillanceArea) {
      val warning: Boolean = broadcast(surveillanceArea, keep(warningDetected, warningRetentionTime, (_: Boolean) == true))
      node.put("warning", warning)
      branch(warning) {
        Cwmpg[Set[Logs]](isDetector, communicationRadius, if (localWarning > 0) Set(Logs(mid)("", timestamp())) else Set.empty, Set.empty,
          accumulate = (ef, t) => t ++ ef.fold(Set.empty)(_++_), extract = (v, w, th, Null) => v)
      }{ Set.empty }
    } { node.put("warning", false); Set.empty }

    val reportingChannel = channel(isDetector, isCollector, channelWidth)
    val reportedLogs:  = branch(reportingChannel){ broadcast(surveillanceArea, logs) }{ Set.empty }

    node.put("logs", logs)
    node.put("meanWarning", meanWarning)
    node.put("meanWarningAtDetector", if(isDetector) meanWarning else 0.0)
    node.put("surveillanceAreaGradient", surveillanceArea)
    node.put("inSurveillanceArea", if(inSurveillanceArea) 1 else 0)
    node.put("localWarningInSystem", localWarning)
    node.put("localWarningInSurveillanceArea", if(inSurveillanceArea) localWarning else 0.0)
    node.put("src", isDetector)
    node.put("dest", isCollector)
    node.put("area", inSurveillanceArea)
    node.put("warningDetected", warningDetected)
    node.put("c", reportingChannel)
    node.put("dataAtCollector", if(isCollector) reportedLogs.size.toDouble else 0.0)
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