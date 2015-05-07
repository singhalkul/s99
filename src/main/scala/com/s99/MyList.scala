package com.s99

import MyList._

/**
 * author: Kul
 */
class MyList[A](self: List[A]) {

  def runLengthEncoding(): List[(Int, A)] = {
    if (self.isEmpty) List()
    else self.pack() myMap { l => (l.size, l.head) }
  }

  def pack(): List[List[A]] = {
    if (self isEmpty) List(Nil)
    else {
      val (packed, rest) = self.mySpan(a => a == self.head)
      (packed, rest) match {
        case (p, Nil) => List(p)
        case (p, r) => p :: r.pack()
      }
    }
  }

  def compress(): List[A] = {
    def compressR(list: List[A], result: List[A]): List[A] = (list, result) match {
      case (Nil, res) => res
      case (h :: tail, Nil) => compressR(tail, h :: Nil)
      case (h :: tail, r :: resTail) if h == r => compressR(tail, result)
      case (h :: tail, r :: resTail) => compressR(tail, h :: result)
    }
    compressR(self, Nil).myReverse
  }

  private def flattenTailRecursive(result: List[A], input: List[A], remainingTail: List[A]): List[A] = input match {
    case Nil if remainingTail == Nil => result
    case Nil => flattenTailRecursive(result, remainingTail, Nil)
    case (l: List[A]) :: tail => flattenTailRecursive(result, l, tail ::: remainingTail)
    case h :: tail => flattenTailRecursive(h :: result, tail, remainingTail)
  }

  private def flattenNonTailRecursive(list: List[A]): List[A] = list match {
    case Nil => Nil
    case (l: List[A]) :: tail => flattenNonTailRecursive(l) ++ flattenNonTailRecursive(tail)
    case h :: tail => h :: flattenNonTailRecursive(tail)
  }

  def myFlatten(): List[A] = flattenTailRecursive(Nil, self, Nil).myReverse()

  def isPalindrome() = {
    def isPalindromeR(list: List[A], reversed: List[A]): Boolean = (list, reversed) match {
      case (Nil, Nil) => true
      case (h1 :: tail1, h2 :: tail2) => h1 == h2 && isPalindromeR(tail1, tail2)
    }
    isPalindromeR(self, self.myReverse)
  }

  def myLength(): Int = {
    def tailLength(list: List[A], result: Int): Int = list match {
      case Nil => result
      case h :: tail => tailLength(tail, result + 1)
    }
    tailLength(self, 0)
  }

  def myReverse(): List[A] = {
    def tailReverse(input: List[A], result: List[A]): List[A] = input match {
      case Nil => result
      case h :: tail => tailReverse(tail, h :: result)
    }
    tailReverse(self, Nil)
  }

  def kth(k: Int): Option[A] = (k, self) match {
    case (p, Nil) => None
    case (0, h :: tail) => Some(h)
    case (n, h :: tail) => tail.kth(n - 1)
  }

  def penultimate(): Option[A] = self match {
    case a :: b :: Nil => Some(a)
    case a :: tail => tail.penultimate()
    case Nil => None
  }

  def myLast(): Option[A] = self match {
    case Nil => None
    case a :: Nil => Some(a)
    case a :: tail => tail.myLast()
  }

  def mySpan(f: A => Boolean) = (self.myTakeWhile(f), self.myDropWhile(f))

  def myTakeWhile(f: A => Boolean) = {
    def takeWhileR(f: A => Boolean)(list: List[A], result: List[A]): List[A] = list match {
      case Nil => result
      case head :: tail if f(head) => takeWhileR(f)(tail, head :: result)
      case head :: tail => result
    }
    takeWhileR(f)(self, Nil)
  }

  def myDropWhile(f: A => Boolean): List[A] = self match {
    case Nil => Nil
    case head :: (tail) if f(head) => tail.myDropWhile(f)
    case head :: tail => head :: tail
  }

  def myMap[B](f: A => B): List[B] = {
    def myMapR(f: A => B, list: List[A], result: List[B]): List[B] = list match {
      case Nil => result
      case head :: tail => myMapR(f, tail, f(head) :: result)
    }
    myMapR(f, self, Nil).reverse
  }
}

object MyList {
  implicit def enhancedList[T](self: List[T]): MyList[T] = new MyList(self)
}