package it.unibo.casestudy

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

trait EdgeFieldsLib {
  self: FieldCalculusSyntax with ExecutionTemplate with StandardSensors with EdgeFields =>

  def broadcast[T](dist: Double, value: T): T = {
    val loc = (dist, value)
    exchange[(Double,T)](loc)(n => {
      (dist, n.withoutSelf.fold[(Double, T)](loc)((t1, t2) => if (t1._1 <= t2._1) t1 else t2)._2)
    })._2
  }

  def distance(source: Boolean, dest: Boolean): Double = {
    broadcast(distanceTo(source, nbrRangeEF), distanceTo(dest, nbrRangeEF))
  }

  def distanceTo(source: Boolean, metric: EdgeField[Double]): Double =
    exchange(Double.PositiveInfinity)(n =>
      mux(source){ 0.0 } { (n + metric).withoutSelf.fold(Double.PositiveInfinity)(Math.min) }
    )

  def channel(source: Boolean, dest: Boolean, width: Double): Boolean = {
    distanceTo(source, nbrRangeEF) + distanceTo(dest, nbrRangeEF) <= distance(source, dest) + width
  }

  /**
   * Weights corresponding to neighbours are calculated in order to penalise devices that are likely to lose
   *   their “receiving” status, a situation that can happen in two cases:
   * (1) if the “receiving” device is too close to the edge of proximity
   *     of the “sending” device, it might step outside of it in the immediate future breaking the connection;
   * (2) if the potential of the “receiving” device is too close to the potential of the “sending” device,
   *     their relative role of sender/receiver might be switched in the immediate future, possibly
   *     creating an “information loop” between the two devices.
   *  w is positive and symmetric.
   */
  def Cwmp(sink: Boolean, radius: Double, value: Double, Null: Double, threshold: Double = 0.1): Double = {
    def accumulate[T : Numeric](v: EdgeField[T], l: T): T = /* E.g., MAX: implicitly[Builtins.Bounded[T]].max(v,l) */
      v.foldSum(l)

    def extract(v: EdgeField[Double], w: EdgeField[Double], threshold: EdgeField[Double], Null: EdgeField[Double]): EdgeField[Double] = //if(w > threshold) v else Null
      v * w

    def weight(dist: Double, radius: Double): EdgeField[Double] = {
      val distDiff: EdgeField[Double] = nbrLocalByExchange(dist).map(v => Math.max(dist-v, 0))
      val res = EdgeField.localToField(radius).map2(nbrRangeEF)(_ - _).map2(distDiff)(_ * _)
      // NB: NaN values may arise when `dist`s are Double.PositiveInfinity (e.g., inf - inf = NaN)
      res.map(v => if(v.isNaN) 0 else v)
    }

    def normalize(w: EdgeField[Double]): EdgeField[Double] = {
      val sum: Double = w.foldSum
      val res = w.map(_ / sum)
      res.map(v => if(v.isNaN) 0 else v)
    }

    val dist = gradient(sink, nbrRangeEF)
    exchange(value)(n => {
      val loc: Double = accumulate(n.withoutSelf, value) // or also: accumulate(selfSubs(n, 0.0),value)
      val w: EdgeField[Double] = weight(dist, radius)
      val normalized: EdgeField[Double] = normalize(w)
      val res: EdgeField[Double] = extract(loc, normalized, threshold, Null)
      selfSubs(res, loc)
    })
  }

  def gradient(source: Boolean, metric: EdgeField[Double]): Double = exchange(Double.PositiveInfinity)(n =>
    mux(source){ 0.0 } { (n + metric).withoutSelf.fold(Double.PositiveInfinity)(Math.min) }
  )

  def nbrRangeEF: EdgeField[Double] = fsns(nbrRange, Double.PositiveInfinity)


  /**
   * - Every node receives by a single parent
   * - Every node transmits to all those neighbours that chose it as a parent
   * - The parent is chosen as the node with minimum distance
   */
  def optimisedBroadcast[T](distance: Double, value: T, Null: T): T = {
    // (default is the self-key)
    val nbrKey: EdgeField[(Double,ID)] = nbrLocalByExchange((distance, mid))
    // `parent` is a Boolean field that holds true for the device that chose the current device as a parent
    //   (default is the true if the self-key is equal to the min-key---this is true only for the source)
    val parent = nbrKey.map(_ == nbrKey.fold[(Double,ID)](nbrKey)((t1,t2) => if(t1._1 < t2._1) t1 else t2))
    exchange(value)(n => {
      val _loc = n.map2(nbrLocalByExchange(distance)){ case (v,d) => (v == Null, d, v) }
        .fold((false, distance, value))((t1,t2) => if(!t1._1 && t2._1) t1 else if(t1._2 < t2._2) t1 else t2)
      val loc = _loc._3
      val nbrParent = nbrByExchange(parent)
      // (default value of `res` is `Null` for every device except sources)
      val res = selfSubs(nbrParent.map(mux(_) { loc } { Null }), loc) // defSubs(..., Null) ?
      // println(s"${mid} => nbrKey ${nbrKey} parent ${parent} N ${n} _loc ${_loc} nbrParent ${nbrParent} res ${res}")
      res
    })
  }

  def hopGradient(src: Boolean): EdgeField[Int] = exchange(Double.PositiveInfinity)(n =>
    mux(src){ 0.0 } { n.withoutSelf.fold(Double.PositiveInfinity)(Math.min) + 1 }
  ).toInt

  def biConnection(): EdgeField[Int] = exchange(0)(n => n + defSubs(1,0))

  def Csubj[P: Builtins.Bounded, V](sink: Boolean, value: V, acc: (V, V) => V, divide: (V,Double) => V): V = {
    val dist: Int = hopGradient(sink)
    // Use exchange to handle communication of distances (dist) and collected values
    exchange[(Int,V)]((dist, value))(n => {
      // The reliability of a downstream neighbor (with lower values of `dist` wrt self) is estimated by the
      //   the corresponding connection time.
      // val conn: EdgeField[Int] = mux(n.map(_._1).fold(Int.MaxValue)(Math.min) < dist){ biConnection() }{ 0 }
      val conn: EdgeField[Int] = pair(n, biConnection()).map{ case (n,biconn) => mux(n._1 < dist){ biconn } { 0 } }
      // Reliability scores are normalised in `send`, obtaining percetanges
      val send: EdgeField[Double] = conn.map(_.toDouble / Math.max(1, conn.foldSum))
      // Let's collect the `send` scores into `recv` scores for receiving neighbours' messages
      val recv: EdgeField[Double] = nbrByExchange(send)
      // Now, values of neighbours (`n.map(_._2)`) are weighted with `recv` scores through given `divide` function
      val weightedValues: EdgeField[V] = pair(n.map(_._2), recv).map(v => divide(v._1, v._2))
      // Finally, use `acc` to aggregate neighbours' contributions
      val collectedValue: V = weightedValues.fold(value)(acc)
      // println(s"${mid} => n = $n dist = ${n._1} conn = $conn send = $send recv = $recv weightedvals = $weightedValues collectedValue = $collectedValue")
      pair(dist : EdgeField[Int], collectedValue)
    })._2
  }

}
