package it.unibo.casestudy

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

class CaseStudy extends AggregateProgram with StandardSensors with ScafiAlchemistSupport with EdgeFields {
  def gradient(source: Boolean, metric: EdgeField[Double]): Double = exchange(Double.PositiveInfinity)(n =>
    mux(source){ 0.0 } { (n + metric).withoutSelf.fold(Double.PositiveInfinity)(Math.min) }
  )

  def nbrRangeEF: EdgeField[Double] = fsns(nbrRange, Double.PositiveInfinity)

  override def main(): Any = {
    val g = gradient(mid==0, nbrRangeEF)
    node.put("g", g)
    g
  }
}