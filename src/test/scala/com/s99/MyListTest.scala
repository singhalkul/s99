package com.s99

import org.scalatest.{Matchers, FunSpec}

/**
 * @author: Kul.
 */
class MyListTest extends FunSpec with Matchers {

  val myList = new MyList

  describe("finding the last element of list") {
    it("should return last element from a list") {
      myList.last(List(1, 2, 3, 4, 5)) should be(Some(5))
    }

    it("should return None") {
      myList.last(Nil) should be(None)
    }
  }

  describe("finding penultimate element of list") {
    it("should return None if list is empty") {
      myList.penultimate(Nil) should be(None)
    }

    it("should return None of list size is 1") {
      myList.penultimate(List(1)) should be(None)
    }

    it("should return the penultimate element from a list of size of 2 elements") {
      myList.penultimate(List(2, 3)) should be(Some(2))
    }

    it("should return the penultimate element from a list of size of 3 elements") {
      myList.penultimate(List(1, 2, 3)) should be(Some(2))
    }

    it("should return the penultimate element from a list of size greater than 3 elements") {
      myList.penultimate(List(1, 2, 3, 4, 5)) should be(Some(4))
    }
  }

  describe("finding kth element of list") {
    it("should return None if list is empty") {
      myList.kth(Nil, 1) should be(None)
    }

    it("should return the kth for list of size 1") {
      myList.kth(List(1), 0) should be(Some(1))
    }

    it("should return the 1st for list of size 2") {
      myList.kth(List(1, 2), 0) should be(Some(1))
    }

    it("should return the 2nd for list of size 2") {
      myList.kth(List(1, 2), 1) should be(Some(2))
    }

    it("should return the 4th for list of size 6") {
      myList.kth(List(1, 2, 3, 4, 5, 6), 3) should be(Some(4))
    }
  }

  describe("reversing a list should reverse") {
    it("Nil for empty list") {
      myList.reverse(Nil) should be(Nil)
    }

    it("list of size 1") {
      myList.reverse(List(1)) should be(List(1))
    }

    it("list of size 2") {
      myList.reverse(List(1, 2)) should be(List(2, 1))
    }

    it("list of size greater than 2") {
      myList.reverse(List(1, 2, 3, 4, 5)) should be(List(5, 4, 3, 2, 1))
    }
  }

  describe("finding length of list") {
    it("should return 0 for empty list") {
      myList.length(Nil) should be(0)
    }

    it("should return 1 for list of size 1") {
      myList.length(List(1)) should be(1)
    }

    it("should return 2 for list of size 2") {
      myList.length(List(1, 2)) should be(2)
    }

    it("should return 5 for list of size 5") {
      myList.length(List(1, 2, 3, 4, 5)) should be(5)
    }
  }

  describe("checking whether a list is palindrome") {
    it("should return true for empty list") {
      myList.isPalindrome(Nil) should be(right = true)
    }

    it("should return true for list with one element") {
      myList.isPalindrome(List(1)) should be(right = true)
    }

    it("should return false for list 1,2") {
      myList.isPalindrome(List(1, 2)) should be(right = false)
    }

    it("should return true for list 1,2,1,2,1") {
      myList.isPalindrome(List(1, 2, 1, 2, 1)) should be(right = true)
    }

    it("should return false for list 1,2,3") {
      myList.isPalindrome(List(1, 2, 3)) should be(right = false)
    }
  }

  describe("flattening a list") {
    it("should return Nil for empty list") {
      myList.flatten(Nil) should be(Nil)
    }

    it("should return same list for List(1,2,3") {
      myList.flatten(List(1, 2, 3)) should be(List(1, 2, 3))
    }

    it("should return flattened list for List(1,List(2,3,4),3)") {
      myList.flatten(List(1, List(2, 3, 4), 3)) should be(List(1, 2, 3, 4, 3))
    }

    it("should return flattened list for List(List(List(2)))") {
      myList.flatten(List(List(List(2)))) should be(List(2))
    }

    it("should return flattened list for List(List(List(1,2)), 3)") {
      myList.flatten(List(List(List(1,2)), 3)) should be(List(1,2,3))
    }
  }

  describe("removing consecutive duplicates from a list") {
    it("should return Nil from empty list") {
      myList.compress(Nil) should be(Nil)
    }

    it("should return List(1) for List(1)") {
      myList.compress(List(1)) should be(List(1))
    }

    it("should return List(1) for List(1,1)") {
      myList.compress(List(1,1)) should be(List(1))
    }

    it("should return List(1,2) for List(1,2)") {
      myList.compress(List(1,2)) should be(List(1,2))
    }

    it("should return compressed list") {
      myList.compress(List(1,1,1,1,2,3,3,1,1,4,5,5,5,5)) should be(List(1,2,3,1,4,5))
    }
  }
}