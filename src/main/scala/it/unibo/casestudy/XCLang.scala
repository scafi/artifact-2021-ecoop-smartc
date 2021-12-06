package it.unibo.casestudy

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import Builtins.Defaultable
import Builtins.Bounded

trait XCLang {
  type NV[T]
  def branch[T](cond: NV[Boolean])(th: => NV[T])(el: => NV[T]): NV[T]
  def exchange[T](init: NV[T])(f: NV[T] => NV[T] /*(NV[T], NV[T])*/): NV[T]
}

/**
 * This trait provides an implementation of the eXchange Calculus (XC).
 */
trait XCLangImpl extends XCLang with NValues with FieldUtils {
    self: FieldCalculusSyntax with ExecutionTemplate =>
  import NValue._

  override type NV[T] = NValue[T]

  override def branch[T](cond: NValue[Boolean])(th: => NValue[T])(el: => NValue[T]): NValue[T] =
    branch(cond.toLocal)(th)(el)

  def exchange[A](init: NValue[A])(f: NValue[A] => NValue[A]): NValue[A] = {
    val rvm = vm.asInstanceOf[RoundVMImpl]
    vm.nest(Scope(s"$EXCHANGE_SLOT_NAME${vm.index}"))(write = true) {
      val nbrs = vm.alignedNeighbours

      def readNeighbourValue[A](id: ID): Option[A] = rvm.context.readSlot(id, rvm.status.path)

      val inputEdgeField = new NValue[A](
        nbrs.map(nbrId => { // create the edgevalue by getting the contributions from all the neighbours (`nbrs`)
          nbrId -> readNeighbourValue[NValue[A]](nbrId) // get edgevalue received from device `nbrId`
            .getOrElse(init).m // if there is not an aligned export from `nbrId`, use `init`
            .getOrElse(vm.self, // from the neighbour's edgevalue, get the value sent to the current device (`self`)
              readNeighbourValue[NValue[A]](nbrId).getOrElse(init).default // otherwise, provide the default
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
}
object XCLangImpl {
  type XCLangSubComponent = XCLangImpl with NValues with FieldUtils with FieldCalculusSyntax with ExecutionTemplate
}
