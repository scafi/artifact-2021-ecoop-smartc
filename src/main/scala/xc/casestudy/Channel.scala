package xc.casestudy

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import xc.{XCLangImpl, XCLib}

class Channel extends AggregateProgram with StandardSensors with ScafiAlchemistSupport with XCLangImpl with XCLib {
  override def main(): Any = {
    val g = gradient(mid==0, nbrRangeEF)
    node.put("g", g)
    node.put("src", mid==0)
    node.put("dest", mid==100)
    val c = channel(mid==0, mid==100, 0.1 /* node.get[Double]("commRadius")*1.5 */)
    node.put("c", c)
    g
  }
}