package com.s99

/**
 * @author: Kul.
 */
object MyList {

  def compress[A](list: List[A]): List[A] = {
    def compressR(list: List[A], result: List[A]): List[A] = (list, result) match {
      case (Nil, res) => res
      case(h :: tail, Nil) => compressR(tail, h :: Nil)
      case (h :: tail, r :: resTail) if h == r => compressR(tail, result)
      case (h :: tail, r :: resTail) => compressR(tail, h :: result)
    }
    reverse(compressR(list, Nil))
  }

  private def flattenTailRecursive[A](result: List[A], input: List[A], remainingTail: List[A]): List[A] = input match {
    case Nil if remainingTail == Nil => result
    case Nil => flattenTailRecursive(result, remainingTail, Nil)
    case (l: List[A]) :: Nil => flattenTailRecursive(result, l, remainingTail)
    case (l: List[A]) :: tail => flattenTailRecursive(result, l, tail ::: remainingTail)
    case h :: tail => flattenTailRecursive(h :: result, tail, remainingTail)
  }

  private def flattenNonTailRecursive[A](list: List[A]): List[A] = list match {
      case Nil => Nil
      case (l : List[A]) :: tail => flattenNonTailRecursive(l) ++ flattenNonTailRecursive(tail)
      case h :: tail => h :: flattenNonTailRecursive(tail)
    }

  def flatten[A](list: List[A]): List[A] = reverse(flattenTailRecursive(Nil, list, Nil))

  def isPalindrome[A](list: List[A]) = {
    def isPalindromeR(list: List[A], reversed: List[A]): Boolean = (list, reversed) match {
      case (Nil, Nil) => true
      case (h1 :: tail1, h2 :: tail2) => h1 == h2 && isPalindromeR(tail1, tail2)
    }
    isPalindromeR(list, reverse(list))
  }

  def length[A](list: List[A]): Int = {
    def tailLength(list: List[A], result: Int): Int = list match {
      case Nil => result
      case h :: tail => tailLength(tail, result + 1)
    }
    tailLength(list, 0)
  }

  def reverse[A](list: List[A]): List[A] = {
    def tailReverse(input: List[A], result: List[A]): List[A] = input match {
      case Nil => result
      case h :: tail => tailReverse(tail, h :: result)
    }
    tailReverse(list, Nil)
  }

  def kth[A](list: List[A], k: Int): Option[A] = (k, list) match {
    case (p, Nil) => None
    case (0, h :: tail) => Some(h)
    case (n, h :: tail) => kth(tail, n - 1)
  }

  def penultimate[A](list: List[A]): Option[A] = list match {
    case a :: b :: Nil => Some(a)
    case a :: l => penultimate(l)
    case a :: Nil => None
    case Nil => None
  }

  def last[A](list: List[A]): Option[A] = {
    list match {
      case Nil => None
      case a :: Nil => Some(a)
      case a :: l => last(l)
    }
  }
}
