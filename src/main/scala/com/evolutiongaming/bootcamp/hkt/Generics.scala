package com.evolutiongaming.bootcamp.hkt

import scala.annotation.tailrec

object Generics {
  case class Triple[+A](x: A, y: A, z: A) {
    def zip[B](other: Triple[B]): Triple[(A, B)] = other match {
      case Triple(ox, oy, oz) => Triple((x, ox), (y, oy), (z, oz))
    } // exercise 1 :  implement

    def set[A1 >: A](index: Triple.Index, value: A1): Triple[A1] = index match {
      case Triple.First => copy(x = value) //Triple(value, y, z)
      case Triple.Second => copy(y = value) // Triple(x, value, z)
      case Triple.Third => copy(z = value) // Triple(x, y, value)
    }

    val vector = Vector(1, 2, 3)
    vector.updated(1, "123")
    //exercise 3 (hard) : fix the definition and implement

  }

  object Triple {

    //
    private def fromLst[A]: PartialFunction[List[A], Triple[A]] = {
      case List(x, y, z) => Triple(x, y, z)
    }

    def fromList[A](elements: List[A]): Option[Triple[A]] = fromLst.lift(elements) // exercise 2 : implement

    sealed trait Index

    case object First extends Index

    case object Second extends Index

    case object Third extends Index
  }


  trait Walker[-A, M, +R] { // exercise 4 : fill in correct variance annotations
    def init: M

    def next(element: A, previous: M): M

    def stop(last: M): R

    def contramap[B](f: B => A): Walker[B, M, R] = ??? // exercice 5 implement
  }


  trait Collection[+A] {
    def walk[M, R](walker: Walker[A, M, R]): R

    //    def map(f: A => B): Collection[B] = ??? // exercise 6 : implement using Walker.contramap

    //    def flatMap(f: A => Collection[B]) : Collection[B] = ??? // HomeWork 2 : implement
  }

  object Collection {
    def apply[A](elements: A*): Collection[A] = new Collection[A] {
      override def walk[State, Result](collector: Walker[A, State, Result]): Result = {
        val it = elements.iterator

        @tailrec def loop(state: State): Result =
          if (it.hasNext) loop(collector.next(it.next(), state))
          else collector.stop(state)

        loop(collector.init)
      }
    } // Homework 1: implement
  }

}


object Subkinding {

  trait Animal

  class Dog extends Animal

  class Cat extends Animal

  type >:>[+A, -B] = <:<[B, A]
  type ???[A, B] = DummyImplicit

  // sub or super 1
  implicitly[ {
    type T[+_]
  } <:< {
    type T[_]
  }]

  // sub or super 2
  implicitly[ {
    type T[_]
  } >:> {
    type T[-_]
  }]

  // sub or super 3
  implicitly[ {
    type T[_, _]
  } >:> {
    type T[-_, +_]
  }]


  // sub or super 4
  implicitly[ {
    type T[_[_]]
  } ??? {
    type T[_]
  }]

  // sub or super 5
  implicitly[ {
    type T[_[_]]
  } <:< {
    type T[_[-_]]
  }]

  // sub or super 6
  implicitly[ {
    type T[_[+_]]
  } ??? {
    type T[_[-_]]
  }]

  // sub or super 7
  implicitly[ {
    type T[_[_[+_]]]
  } <:< {
    type T[_[_[_]]]
  }]

  // sub or super 8
  implicitly[ {
    type T[_ >: Dog <: Animal]
  } >:> {
    type T[_]
  }]

  // sub or super 9
  implicitly[ {
    type T[_[_ >: Dog <: Animal]]
  } <:< {
    type T[_[_]]
  }]


  class FooIt[t[x] <: Iterable[x]]

  class Foo[t[x]]

  class Accepts[foo[t[x]]]

  class AcceptsIt[foo[t[x] <: Iterable[x]]]

  type T = (Accepts[Foo], AcceptsIt[FooIt], Accepts[FooIt], AcceptsIt[Foo])


  // sub or super 10
  implicitly[ {
    type T[_[x] <: Iterable[x]]
  } >:> {
    type T[_[_]]
  }]

}
