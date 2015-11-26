package com.s99

import MyList._

/**
 * author: Kul
 */
class MyList[A](self: List[A]) {

  def duplicateN(times: Int) = self myFlatMap { e => List.fill(times)(e) }

  def duplicate = self myFlatMap (e => List(e, e))

  def encodeDirect(): List[(Int, A)] = {
    if (self isEmpty) Nil
    else {
      val (packed, rest) = self.mySpan(_ == self.head)
      (packed, rest) match {
        case (p, Nil) => List((p.size, p.head))
        case (p, r) => (p.size, p.head) :: r.encodeDirect()
      }
    }
  }

  def runLengthEncoding() = {
    if (self.isEmpty) List()
    else self.pack() myMap { l => (l.size, l.head) }
  }

  def runLengthEncodingModified() = {
    if (self.isEmpty) List()
    else self.pack() myMap { l => if (l.size == 1) l.head else (l.size, l.head) }
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
    compressR(self, Nil).myReverse()
  }

  private def flattenTailRecursive(result: List[A], input: List[A], remainingTail: List[A]): List[A] = input match {
    case Nil if remainingTail == Nil => result
    case Nil => flattenTailRecursive(result, remainingTail, Nil)
    case (l: List[A]) :: tail => flattenTailRecursive(result, l, tail ::: remainingTail)
    case h :: tail => flattenTailRecursive(h :: result, tail, remainingTail)
  }

  def myFlatten(): List[A] = flattenTailRecursive(Nil, self, Nil).myReverse()

  def isPalindrome = {
    def isPalindromeR(list: List[A], reversed: List[A]): Boolean = (list, reversed) match {
      case (Nil, Nil) => true
      case (Nil, _) | (_, Nil) => false
      case (h1 :: tail1, h2 :: tail2) => h1 == h2 && isPalindromeR(tail1, tail2)
    }
    isPalindromeR(self, self.myReverse())
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

  def dropN(n: Int) = self.myZipWithIndex.myFilter(t => (t._2 + 1) % n != 0).map(_._1)

  def myLast(): Option[A] = self match {
    case Nil => None
    case a :: Nil => Some(a)
    case a :: tail => tail.myLast()
  }

  def rotateLeft(n: Int): List[A] = if(n < 0) rotateLeft(self.size - n.abs) else self.myDrop(n) ::: self.myTake(n)

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

  def myFlatMap[B](f: A => Iterable[B]): Iterable[B] = {

    def myFlatMapR(f: A => Iterable[B], list: List[A], result: Iterable[B]): Iterable[B] = {
      list match {
        case Nil => result
        case head :: tail => myFlatMapR(f, tail, result ++ f(head))
      }
    }
    myFlatMapR(f, self, Nil)
  }

  def myGrouped(group: Int): List[List[A]] = {

    def groupedR(list: List[A], acc: List[A], count: Int, result: List[List[A]]): List[List[A]] = list match {
      case Nil => List(acc)
      case head :: Nil if group == count => List(acc) ++ List(List(head))
      case head :: Nil => List(acc :+ head)
      case head :: tail if group == count => groupedR(tail, head :: Nil, 0, result ++ List(acc))
      case head :: tail => groupedR(tail, acc :+ head, count + 1, result)
    }
    groupedR(self, Nil, 0, Nil)
  }

  def myFilter(f: A => Boolean): List[A] = {

    def myFilterR(f: A => Boolean, list: List[A], result: List[A]): List[A] = list match {
      case Nil => result
      case head :: tail => if(f(head)) myFilterR(f, tail, head :: result) else myFilterR(f, tail, result)
    }

    myFilterR(f, self, Nil).myReverse()
  }

  def myZipWithIndex: List[(A, Int)] = {
    def myZipWithIndexR(list: List[A], index: Int, result: List[(A, Int)]): List[(A, Int)] = list match {
      case Nil => result
      case head :: tail => myZipWithIndexR(tail, index + 1, (head, index) :: result)
    }
    myZipWithIndexR(self, 0, Nil).myReverse()
  }

  def mySplit(n: Int): (List[A], List[A]) = {

    def mySplitR(n: Int, list: List[A], result: List[A]): (List[A], List[A]) = {
      (n, list) match {
        case (0, _) => (result, list)
        case (_, Nil) => (result, Nil)
        case (_, head :: Nil) => (head :: result, Nil)
        case (_, head :: tail) => mySplitR(n - 1, tail, head :: result)
      }
    }
    val (first, second) = mySplitR(n, self, Nil)
    (first.myReverse(), second)
  }

  def mySlice(from: Int, to: Int) = {
    self.myDrop(from).myTake(to - from)
  }

  def myTake(n : Int) = {
    def myTakeR(n: Int, list: List[A], result: List[A]): List[A] = {
      (n, list) match {
        case (0, _) | (_, Nil) => result
        case (_, head :: tail) => myTakeR(n - 1, tail, head :: result)
      }
    }
    if (n <= 0) Nil else myTakeR(n, self, Nil).myReverse()
  }

  def myDrop(n: Int) = {
    def myDropR(n: Int, list: List[A], result: List[A]): List[A] = {
      (n, list) match {
        case (0, _) | (_, Nil)=> result
        case(_, head :: tail) => myDropR(n - 1, tail, tail)
      }
    }
    if(n <= 0) self else myDropR(n, self, Nil)
  }

  def removeKthElement(k: Int) = {
    val (first, second) = self.mySplit(k)
    (first ::: second.tail, second. head)
  }

  def insertAt(k: Int, a: A) = {
    val (first, second) = self.mySplit(k)
    first ::: a :: second
  }



}

object MyList {
  implicit def enhancedList[T](self: List[T]): MyList[T] = new MyList(self)

  def createListForRange(from: Int, to: Int) = {

    def createR(from: Int, to: Int, result: List[Int]): List[Int] = {
      if(from == to) from :: result
      else createR(from, to - 1, to :: result)
    }
    createR(from, to, Nil)
  }
}