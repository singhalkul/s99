package com.s99

import org.scalatest.{Matchers, FunSpec}

/**
 * @author: Kul.
 */
class MyListTest extends FunSpec with Matchers {

  describe("finding the last element of list") {
    it("should return last element from a list") {
      MyList.last(List(1, 2, 3, 4, 5)) should be(Some(5))
    }

    it("should return None") {
      MyList.last(Nil) should be(None)
    }
  }

  describe("finding penultimate element of list") {
    it("should return None if list is empty") {
      MyList.penultimate(Nil) should be(None)
    }

    it("should return None of list size is 1") {
      MyList.penultimate(List(1)) should be(None)
    }

    it("should return the penultimate element from a list of size of 2 elements") {
      MyList.penultimate(List(2, 3)) should be(Some(2))
    }

    it("should return the penultimate element from a list of size of 3 elements") {
      MyList.penultimate(List(1, 2, 3)) should be(Some(2))
    }

    it("should return the penultimate element from a list of size greater than 3 elements") {
      MyList.penultimate(List(1, 2, 3, 4, 5)) should be(Some(4))
    }
  }

  describe("finding kth element of list") {
    it("should return None if list is empty") {
      MyList.kth(Nil, 1) should be(None)
    }

    it("should return the kth for list of size 1") {
      MyList.kth(List(1), 0) should be(Some(1))
    }

    it("should return the 1st for list of size 2") {
      MyList.kth(List(1, 2), 0) should be(Some(1))
    }

    it("should return the 2nd for list of size 2") {
      MyList.kth(List(1, 2), 1) should be(Some(2))
    }

    it("should return the 4th for list of size 6") {
      MyList.kth(List(1, 2, 3, 4, 5, 6), 3) should be(Some(4))
    }
  }

  describe("reversing a list should reverse") {
    it("Nil for empty list") {
      MyList.reverse(Nil) should be(Nil)
    }

    it("list of size 1") {
      MyList.reverse(List(1)) should be(List(1))
    }

    it("list of size 2") {
      MyList.reverse(List(1, 2)) should be(List(2, 1))
    }

    it("list of size greater than 2") {
      MyList.reverse(List(1, 2, 3, 4, 5)) should be(List(5, 4, 3, 2, 1))
    }
  }

  describe("finding length of list") {
    it("should return 0 for empty list") {
      MyList.length(Nil) should be(0)
    }

    it("should return 1 for list of size 1") {
      MyList.length(List(1)) should be(1)
    }

    it("should return 2 for list of size 2") {
      MyList.length(List(1, 2)) should be(2)
    }

    it("should return 5 for list of size 5") {
      MyList.length(List(1, 2, 3, 4, 5)) should be(5)
    }
  }

  describe("checking whether a list is palindrome") {
    it("should return true for empty list") {
      MyList.isPalindrome(Nil) should be(right = true)
    }

    it("should return true for list with one element") {
      MyList.isPalindrome(List(1)) should be(right = true)
    }

    it("should return false for list 1,2") {
      MyList.isPalindrome(List(1, 2)) should be(right = false)
    }

    it("should return true for list 1,2,1,2,1") {
      MyList.isPalindrome(List(1, 2, 1, 2, 1)) should be(right = true)
    }

    it("should return false for list 1,2,3") {
      MyList.isPalindrome(List(1, 2, 3)) should be(right = false)
    }
  }

  describe("flattening a list") {
    it("should return Nil for empty list") {
      MyList.flatten(Nil) should be(Nil)
    }

    it("should return same list for List(1,2,3") {
      MyList.flatten(List(1, 2, 3)) should be(List(1, 2, 3))
    }

    it("should return flattened list for List(1,List(2,3,4),3)") {
      MyList.flatten(List(1, List(2, 3, 4), 3)) should be(List(1, 2, 3, 4, 3))
    }

    it("should return flattened list for List(List(List(2)))") {
      MyList.flatten(List(List(List(2)))) should be(List(2))
    }

    it("should return flattened list for List(List(List(1,2)), 3)") {
      MyList.flatten(List(List(List(1,2)), 3)) should be(List(1,2,3))
    }
  }

  describe("removing consecutive duplicates from a list") {
    it("should return Nil from empty list") {
      MyList.compress(Nil) should be(Nil)
    }

    it("should return List(1) for List(1)") {
      MyList.compress(List(1)) should be(List(1))
    }

    it("should return List(1) for List(1,1)") {
      MyList.compress(List(1,1)) should be(List(1))
    }

    it("should return List(1,2) for List(1,2)") {
      MyList.compress(List(1,2)) should be(List(1,2))
    }

    it("should return compressed list") {
      MyList.compress(List(1,1,1,1,2,3,3,1,1,4,5,5,5,5)) should be(List(1,2,3,1,4,5))
    }
  }
}