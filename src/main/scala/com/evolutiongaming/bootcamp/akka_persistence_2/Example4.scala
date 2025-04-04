package com.evolutiongaming.bootcamp.akka_persistence_2

import akka.actor.{ActorSystem, Props}
import akka.persistence.{PersistentActor, RecoveryCompleted, SnapshotOffer}

case class EmployeeBasket(
  employeeId: String,
  items: List[String]
)

class BasketAggregateBehaviour extends AggregateBehaviour[EmployeeBasket, BasketCommand, BasketEvent, BasketError] {
  val maxSize = 5

  override def processCommand(state: EmployeeBasket): BasketCommand => Either[BasketError, List[BasketEvent]] =
    {
      case AddItem(_, item) if state.items.size < maxSize => Right(List(ItemAdded(item)))
      case AddItem(_, _)                                  => Left(BasketFull)
    }

  override def applyEvent(state: EmployeeBasket): BasketEvent => EmployeeBasket = {
    case ItemAdded(item) => state.copy(items = item :: state.items)
  }
}

object Example4 extends App {


  class EmployeeShoppingBasketActor extends PersistentActor {

    def id = self.path.name
    println(id)

    val behaviour = new BasketAggregateBehaviour

    private var basket = EmployeeBasket(id, Nil)

    override def receiveRecover: Receive = {
      case SnapshotOffer(metadata, basketFromSnapshot: EmployeeBasket) =>
        println(s"Snapshot $basketFromSnapshot")
        basket = basketFromSnapshot

      case event: BasketEvent =>
        println(s"Recovering $event")
        basket = behaviour.applyEvent(basket)(event)
    }

    override def receiveCommand: Receive = {
      case cmd: BasketCommand =>
        behaviour.processCommand(basket)(cmd) match {
          case Left(rejection) =>
            println(s"Rejected with $rejection")
            sender() ! rejection
          case Right(event :: _) =>
            println(s"Received $event in $id")
            persist(event) { event =>
              basket = behaviour.applyEvent(basket)(event)
              if (lastSequenceNr % 5 == 0) saveSnapshot(basket)
            }
        }
    }

    // user id, we will see how to deal with it in future
    override def persistenceId: String = self.path.name

  }


  val system = ActorSystem("AkkaPersistenceSystem")
  val basket = system.actorOf(Props[EmployeeShoppingBasketActor]())

  basket ! AddItem("1", "banana")
  basket ! AddItem("1", "apple")
  basket ! AddItem("1", "orange")
  basket ! AddItem("1", "avocado")
  basket ! AddItem("1", "cherries") // <-- snapshot should be created at this point
  basket ! AddItem("1", "guava")

}
