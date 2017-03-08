package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(integers: List[Int]): Int = integers match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  def setHead[A](head:A, ofList: List[A]): List[A] = ofList match {
    case Nil => List(head)
    case Cons(_, tail) => Cons(head, tail)
  }

  @tailrec
  def drop[A](numberOfElements: Int, fromList: List[A]): List[A] = {
    if(numberOfElements == 0) fromList
    else{
      fromList match {
        case Nil => Nil
        case Cons(_, tail) => drop(numberOfElements - 1, tail)
      }
    }
  }

  @tailrec
  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Cons(head, tail) if (f(head)) => dropWhile(tail)(f)
    case _ => as
  }

  def append[A](a: List[A], b: List[A]): List[A] = {
    a match {
      case Nil => b
      case Cons(h, t) => Cons(h, append(t, b))
    }
  }

  def init[A](l: List[A]): List[A] = {
    @tailrec
    def _init(l: List[A], acc: List[A]): List[A] = l match {
      case Nil => acc
      case Cons(_, Nil) => acc
      case Cons(h, t) => _init(t, append(acc, List(h)))
    }

    _init(l, Nil)
  }
}
