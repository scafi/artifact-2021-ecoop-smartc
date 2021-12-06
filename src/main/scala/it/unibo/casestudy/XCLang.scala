package it.unibo.casestudy

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import Builtins.Defaultable
import Builtins.Bounded

trait XCLang {
  type NV[T]
  def branch[T](cond: NV[Boolean])(th: => NV[T])(el: => NV[T]): NV[T]
  def exchange[T](init: NV[T])(f: NV[T] => NV[T] /*(NV[T], NV[T])*/): NV[T]
}

trait XCLangImpl extends XCLang with FieldUtils {
    self: FieldCalculusSyntax with ExecutionTemplate =>
  import NValue._

  override type NV[T] = NValue[T]

  override def branch[T](cond: NValue[Boolean])(th: => NValue[T])(el: => NValue[T]): NValue[T] =
    branch(cond.toLocal)(th)(el)

  def exchange[A](init: NValue[A])(f: NValue[A] => NValue[A]): NValue[A] = {
    val rvm = vm.asInstanceOf[RoundVMImpl]
    vm.nest(Scope(s"$EXCHANGE_SLOT_NAME${vm.index}"))(write = true) {
      val nbrs = vm.alignedNeighbours

      def nbrEdgeValue[A](id: ID): Option[A] = rvm.context.readSlot(id, rvm.status.path)

      val inputEdgeField = new NValue[A](
        nbrs.map(nbrId => { // create the edgevalue by getting the contributions from all the neighbours (`nbrs`)
          nbrId -> nbrEdgeValue[NValue[A]](nbrId) // get edgevalue received from device `nbrId`
            .getOrElse(init).m // if there is not an aligned export from `nbrId`, use `init`
            .getOrElse(vm.self, // from the neighbour's edgevalue, get the value sent to the current device (`self`)
              nbrEdgeValue[NValue[A]](nbrId).getOrElse(init).default // otherwise, provide the default
            )
        }).toMap,
        init.default)
      f(inputEdgeField)
    }
  }

  def exchangeFull[A](init: NValue[A])(f: ExchangeParams[A] => NValue[A]): NValue[A] = {
    val rvm = vm.asInstanceOf[RoundVMImpl]
    vm.nest(Scope(s"$EXCHANGE_SLOT_NAME${vm.index}"))(write = true) {
      val nbrs = vm.alignedNeighbours

      def nbrEdgeValue[A](id: ID): Option[A] = rvm.context.readSlot(id, rvm.status.path)

      val oldEdgeField = rvm.context.readSlot(vm.self, rvm.status.path).getOrElse(init)
      val inputEdgeField = new NValue[A](
        nbrs.map(nbrId =>
          nbrId -> nbrEdgeValue[NValue[A]](nbrId)
            .getOrElse(init).m
            .getOrElse(vm.self, nbrEdgeValue[NValue[A]](nbrId).getOrElse(init).default)
        ).toMap,
        init.default)
      val outputEdgeField = f(ExchangeParams(oldEdgeField, inputEdgeField))
      outputEdgeField
    }
  }

  case class ExchangeParams[T](old: NValue[T], neigh: NValue[T])

  object ExchangeParams {
    implicit def fromTuple[T](tuple: (NValue[T], NValue[T])): ExchangeParams[T] =
      ExchangeParams(tuple._1, tuple._2)
  }

  private val EXCHANGE_SLOT_NAME = Scope("exchange")
  private val EXCHANGE_SLOT = Scope(EXCHANGE_SLOT_NAME)

  def defSubs[T](ef: NValue[T], defaultValue: T): NValue[T] =
    NValue(vm.alignedNeighbours().map(id => id -> ef.m.getOrElse(id, ef.default)).toMap, defaultValue)

  def selfSubs[T](ef: NValue[T], selfValue: T): NValue[T] =
    NValue(ef.m ++ Map[ID, T](mid -> selfValue), ef.default)

  def nbrByExchange[A](e: => NValue[A]): NValue[A] =
    exchange[(A, A)](e.map2(e)((_, _)))(n => n.map2(e) { case (n, e) => (e, n._1) }).map(_._2) // NB: exchange(..)(..)._2 would compile but doesn't work


  def nbrLocalByExchange[A](e: => A): NValue[A] =
    exchange[(A, NValue[A])]((e, NValue.localToField(e)))(n => (e, n.map(_._1)))._2

  def repByExchange[A](init: => A)(f: (A) => A): A =
    exchangeFull(init)(p => f(p.old))

  def shareByExchange[A](init: A)(f: A => A): A =
    exchange(init)(n => f(n))

  // exchangeFull(init)(p => f(p.neigh))

  def fsns[A](e: => A, defaultA: A): NValue[A] =
    NValue[A](includingSelf.reifyField(e), defaultA)

  def pair[A, B](a: NValue[A], b: NValue[B]): NValue[(A, B)] =
    a.map2(b)((_, _))

  /**
   * Basic Field type
   *
   * @param m map from devices to corresponding values
   * @tparam T type of field values
   */
  class NValue[T](val m: Map[ID, T], override val default: T) extends Builtins.Defaultable[T] {
    implicit val defaultable: Defaultable[T] = this

    def defSubs(defaultValue: T): NValue[T] =
      NValue[T](this.m, defaultValue)

    def selfSubs(selfValue: T): NValue[T] =
      NValue[T](this.m ++ Map[ID, T](mid -> selfValue), this.default)

    def restricted: NValue[T] = { // TODO: contains on list {
      val nbrsSet = vm.alignedNeighbours().toSet
      NValue(this.m.filter(el => nbrsSet.contains(el._1)), this.default)
    }

    def flatMap[R](f: T => NValue[R]): NValue[R] =
      NValue(this.m.map { case (id, v) => id -> {
        val newEdgeField = f(v)
        newEdgeField.m.getOrElse(id, newEdgeField.default)
      }
      }, f(this.default))

    def mapWithId[R](o: (Option[ID], T) => R): NValue[R] =
      NValue(this.m.map(tp => tp._1 -> o(Some(tp._1), tp._2)), o(None, default))

    def map[R](o: T => R): NValue[R] =
      NValue(this.m.map(tp => tp._1 -> o(tp._2)), o(default))

    def map[R](defaultr: R, o: T => R): NValue[R] =
      NValue(this.m.map(tp => tp._1 -> o(tp._2)), defaultr)

    def map2[R, S](f: NValue[R])(o: (T, R) => S): NValue[S] =
      NValue((this.m.keySet ++ f.m.keySet).map(i => i -> o(this.m.getOrElse(i, this.default), f.m.getOrElse(i, f.default))).toMap,
        o(default, f.default))

    def map2l[R, S](f: NValue[R])(o: (T, R) => S): NValue[S] =
      NValue(this.m.map { case (i, v) => i -> o(v, f.m.getOrElse(i, f.default)) }, o(default, f.default))

    def map2r[R, S](f: NValue[R])(o: (T, R) => S): NValue[S] =
      NValue(f.m.map { case (i, v) => i -> o(this.m.getOrElse(i, this.default), v) }, o(default, f.default))

    def map2i[R, S](f: NValue[R])(o: (T, R) => S): NValue[S] =
      map2u(f)(this.default, f.default)(o)

    def map2d[R, S](f: NValue[R])(defaultr: R)(o: (T, R) => S): NValue[S] =
      NValue(this.m.map { case (i, v) => i -> o(v, f.m.getOrElse(i, defaultr)) }, o(default, defaultr))

    def map2u[R, S](f: NValue[R])(dl: T, dr: R)(o: (T, R) => S): NValue[S] =
      NValue((this.m.keys ++ f.m.keys).map { k => k -> o(this.m.getOrElse(k, dl), f.m.getOrElse(k, dr)) }.toMap,
        o(dl, dr))

    def fold[V >: T](z: V)(o: (V, V) => V): V =
      restricted.m.values.fold(z)(o)

    def reduce[V >: T](o: (V, V) => V): V =
      restricted.m.values.reduce(o)

    def minHood[V >: T](implicit ev: Bounded[V]): V =
      fold[V](ev.top) { case (a, b) => ev.min(a, b) }

    def minHoodPlus[V >: T](implicit ev: Bounded[V]): V =
      withoutSelf.minHood(ev)

    def withoutSelf: NValue[T] = NValue[T](this.m - mid, this.default)

    def toLocal: T = NValue.fieldToLocal(this)

    def toMap: Map[ID, T] = this.m

    override def toString: String = s"EdgeField[default=$default, exceptions=$m]"
  }

  object NValue {

    def apply[T](m: Map[ID, T])(implicit defaultable: Builtins.Defaultable[T]): NValue[T] =
      apply(m, defaultable.default)

    def apply[T](m: Map[ID, T], defaultValue: T): NValue[T] =
      new NValue(m, defaultValue)

    implicit def localToField[T](lv: T): NValue[T] =
      NValue(Map.empty, lv)

    implicit def fieldToLocal[T](fv: NValue[T]): T = {
      fv.m.getOrElse(mid, fv.default)
    }
  }

  /**
   * Syntactic sugar for numeric fields.
   */
  implicit class NumericNValue[T: Numeric](f: NValue[T]) extends Defaultable[T] {
    private val ev = implicitly[Numeric[T]]

    override def default: T = f.default

    implicit val defaultable: Defaultable[T] = this
    implicit val defaultableDouble: Defaultable[Double] = new Defaultable[Double] {
      override def default: Double = 0.0
    }

    def +(f2: NValue[T]): NValue[T] = f.map2i(f2)(ev.plus(_, _))

    def -(f2: NValue[T]): NValue[T] = f.map2i(f2)(ev.minus(_, _))

    def *(f2: NValue[T]): NValue[T] = f.map2i(f2)(ev.times(_, _))

    def +/[U](lv: U)(implicit uev: Numeric[U]): NValue[Double] = f.map[Double](ev.toDouble(_: T) + uev.toDouble(lv))

    def foldSum(init: T): NValue[T] = f.fold(init)(ev.plus)

    def foldSum(): NValue[T] = foldSum(ev.zero)
  }

  implicit class NValueOfTuples[A, B](f: NValue[Tuple2[A, B]]) {
    def _1: NValue[A] = f.map(_._1)

    def _2: NValue[B] = f.map(_._2)
  }

}
