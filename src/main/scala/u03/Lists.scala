package u03

import u02.Modules.Person

import scala.annotation.tailrec
import u02.Optionals.Option.*
import u02.Modules.Person.*

object Lists extends App:

  // A generic linkedlist
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()
  // a companion object (i.e., module) for List
  object List:

    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def map2[A, B](l: List[A])(mapper: A => B): List[B] =
      flatMap(l)(f => Cons(mapper(f), Nil()))

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    def filter2[A](l: List[A])(pred: A => Boolean): List[A] =
      flatMap(l)(f => pred(f) match
        case true => Cons(f, Nil())
        case false => Nil())

    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = l match
      case Cons(h, l) if n > 0 => drop(l, n - 1)
      case _ => l

    def append[A](left: List[A], right: List[A]): List[A] = left match
      case Cons(h, t) if t != Nil() => Cons(h, append(t, right))
      case Cons(h, t) => Cons(h, right)
      case _ => Nil()

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
      case Cons(h, t) if f(h) != Nil() => append(f(h), flatMap(t)(f))
      case Cons(h, t) => flatMap(t)(f)
      case _ => Nil()

    def max(l: List[Int]): u02.Optionals.Option[Int] = l match
      case Cons(h, t) if orElse(max(t), h) > h => max(t)
      case Cons(h, t) => Some(h)
      case _ => None()

    def getCourses(l: List[Person]): List[String] =
      flatMap(l)(p => p match
        case Teacher(n, c) => Cons(c, Nil())
        case _ => Nil())

    def foldLeft[A, B](l: List[A])(default: B)(acc: (B, A) => B): B =
      @tailrec
      def _foldLeft[A, B](l: List[A])(acc: (B, A) => B)(tmp: B): B = l match
        case Cons(h, t) => _foldLeft(t)(acc)(acc(tmp, h))
        case _ => tmp

      _foldLeft(l)(acc)(default)

    def foldRight[A, B](l: List[A])(default: B)(acc: (B, A) => B): B =
      def _reverse(l: List[A]): List[A] = ???

      foldLeft(_reverse(l))(default)(acc)

  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60

  import List.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
