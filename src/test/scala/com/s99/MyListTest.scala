package com.s99

import MyList._
import org.scalatest.{Matchers, FunSpec}

/**
 * author: Kul.
 */
class MyListTest extends FunSpec with Matchers {

  describe("Scala 99 problems") {

    describe("01. finding the last element of list") {
      it("should return last element from a list") {
        List(1, 2, 3, 4, 5).myLast() should be(Some(5))
      }

      it("should return None") {
        Nil.myLast() should be(None)
      }
    }

    describe("02. finding penultimate element of list") {
      it("should return None if list is empty") {
        Nil.penultimate() should be(None)
      }

      it("should return None of list size is 1") {
        List(1).penultimate() should be(None)
      }

      it("should return the penultimate element from a list of size of 2 elements") {
        List(2, 3).penultimate() should be(Some(2))
      }

      it("should return the penultimate element from a list of size of 3 elements") {
        List(1, 2, 3).penultimate() should be(Some(2))
      }

      it("should return the penultimate element from a list of size greater than 3 elements") {
        List(1, 2, 3, 4, 5).penultimate() should be(Some(4))
      }
    }

    describe("03. finding kth element of list") {
      it("should return None if list is empty") {
        Nil.kth(1) should be(None)
      }

      it("should return the kth for list of size 1") {
        List(1).kth(0) should be(Some(1))
      }

      it("should return the 1st for list of size 2") {
        List(1, 2).kth(0) should be(Some(1))
      }

      it("should return the 2nd for list of size 2") {
        List(1, 2).kth(1) should be(Some(2))
      }

      it("should return the 4th for list of size 6") {
        List(1, 2, 3, 4, 5, 6).kth(3) should be(Some(4))
      }
    }

    describe("04. reversing a list should reverse") {
      it("Nil for empty list") {
        Nil.myReverse() should be(Nil)
      }

      it("list of size 1") {
        List(1).myReverse() should be(List(1))
      }

      it("list of size 2") {
        List(1, 2).myReverse() should be(List(2, 1))
      }

      it("list of size greater than 2") {
        List(1, 2, 3, 4, 5).myReverse() should be(List(5, 4, 3, 2, 1))
      }
    }

    describe("05. finding length of list") {
      it("should return 0 for empty list") {
        Nil.myLength() should be(0)
      }

      it("should return 1 for list of size 1") {
        List(1).myLength() should be(1)
      }

      it("should return 2 for list of size 2") {
        List(1, 2).myLength() should be(2)
      }

      it("should return 5 for list of size 5") {
        List(1, 2, 3, 4, 5).myLength() should be(5)
      }
    }

    describe("06. checking whether a list is palindrome") {
      it("should return true for empty list") {
        Nil.isPalindrome should be(right = true)
      }

      it("should return true for list with one element") {
        List(1).isPalindrome should be(right = true)
      }

      it("should return false for list 1,2") {
        List(1, 2).isPalindrome should be(right = false)
      }

      it("should return true for list 1,2,1,2,1") {
        List(1, 2, 1, 2, 1).isPalindrome should be(right = true)
      }

      it("should return false for list 1,2,3") {
        List(1, 2, 3).isPalindrome should be(right = false)
      }
    }

    describe("07. flattening a list") {
      it("should return Nil for empty list") {
        Nil.myFlatten() should be(Nil)
      }

      it("should return same list for List(1,2,3") {
        List(1, 2, 3).myFlatten() should be(List(1, 2, 3))
      }

      it("should return flattened list for List(1,List(2,3,4),3)") {
        List(1, List(2, 3, 4), 3).myFlatten() should be(List(1, 2, 3, 4, 3))
      }

      it("should return flattened list for List(List(List(2)))") {
        List(List(List(2))).myFlatten() should be(List(2))
      }

      it("should return flattened list for List(List(List(1,2)), 3)") {
        List(List(List(1, 2)), 3).myFlatten() should be(List(1, 2, 3))
      }
    }

    describe("08. removing consecutive duplicates from a list") {
      it("should return Nil from empty list") {
        Nil.compress() should be(Nil)
      }

      it("should return List(1) for List(1)") {
        List(1).compress() should be(List(1))
      }

      it("should return List(1) for List(1,1)") {
        List(1, 1).compress() should be(List(1))
      }

      it("should return List(1,2) for List(1,2)") {
        List(1, 2).compress() should be(List(1, 2))
      }

      it("should return compressed list") {
        List(1, 1, 1, 1, 2, 3, 3, 1, 1, 4, 5, 5, 5, 5).compress() should be(List(1, 2, 3, 1, 4, 5))
      }
    }

    describe("09. packing consecutive duplicates") {

      it("should return List of Nil for empty List") {
        Nil.pack() should be(List(Nil))
      }

      it("should return List(List(1)) for List(1)") {
        List(1).pack() should be(List(List(1)))
      }

      it("should return List(List(1), List(2)) for List(1,2)") {
        List(1, 2).pack() should be(List(List(1), List(2)))
      }

      it("should return List(List(1,1,1), List(2,2), List(1)) for List(1,1,1,2,2,1)") {
        List(1, 1, 1, 2, 2, 1).pack() should be(List(List(1, 1, 1), List(2, 2), List(1)))
      }
    }

    describe("10. run length encoding of duplicate elements in a list") {

      it("should return List of Nil for empty list") {
        Nil.runLengthEncoding() should be(Nil)
      }

      it("should return List(List((1, 1))) for List(1)") {
        List(1).runLengthEncoding() should be(List((1, 1)))
      }

      it("should return List((3, 1), (1, 2), (2, 3)) for List(1,1,1,2,3,3)") {
        List(1, 1, 1, 2, 3, 3).runLengthEncoding() should be(List((3, 1), (1, 2), (2, 3)))
      }
    }

    describe("11. modified run length encoding of duplicate elements in a list") {

      it("should return Nil for empty list") {
        Nil.runLengthEncodingModified() should be(Nil)
      }

      it("should return List(1) for List(1)") {
        List(1).runLengthEncodingModified() should be(List(1))
      }

      it("should return List((2, 1)) for List(1, 1)") {
        List(1, 1).runLengthEncodingModified() should be(List((2, 1)))
      }

      it("should return List((2, 1), 3) for List(1, 1, 3)") {
        List(1, 1, 3).runLengthEncodingModified() should be(List((2, 1), 3))
      }

      it("should return List((3, 1), 2, (2, 3)) for List(1,1,1,2,3,3)") {
        List(1, 1, 1, 2, 3, 3).runLengthEncodingModified() should be(List((3, 1), 2, (2, 3)))
      }
    }

    describe("12. Decode a run-length encoded list") {

      def decode[A](t: (Int, A)) = List.fill(t._1)(t._2)

      it("should return List(2) for List((1, 2))") {
        List((1, 2)).myFlatMap(decode) should be(List(2))
      }

      it("should return List(2, 3, 4, 5) for List((1, 2), (2, 3), (2, 5))") {
        List((1, 2), (2, 3), (2, 5)).myFlatMap(decode) should be(List(2, 3, 3, 5, 5))
      }
    }

    describe("13. direct run length encoding (without using pack method from previous example) of duplicate elements in a list") {

      it("should return Nil for empty list") {
        Nil.encodeDirect() should be(Nil)
      }

      it("should return List(List((1, 1))) for List(1)") {
        List(1).encodeDirect() should be(List((1, 1)))
      }

      it("should return List((3, 1), (1, 2), (2, 3)) for List(1,1,1,2,3,3)") {
        List(1, 1, 1, 2, 3, 3).encodeDirect() should be(List((3, 1), (1, 2), (2, 3)))
      }
    }

    describe("14. duplicate the elements of a list") {

      it("should return List(1, 1) for List(1)") {
        List(1).duplicate should be(List(1, 1))
      }

      it("should return List(1, 1, 3, 3) for List(1, 3)") {
        List(1, 3).duplicate should be(List(1, 1, 3, 3))
      }
    }

    describe("15. duplicate the elements of a list given number of times") {

      it("should return List(1, 1) for List(1) duplicated 2 times") {
        List(1).duplicateN(2) should be(List(1, 1))
      }

      it("should return List(1, 1, 1, 3, 3, 3) for List(1, 3) duplicated 3 times") {
        List(1, 3).duplicateN(3) should be(List(1, 1, 1, 3, 3, 3))
      }
    }

    describe("16. Drop every Nth element from a list") {
      it("should return List(1, 3, 5) for List(1, 2, 3, 4, 5, 6) after dropping every 2nd element") {
        List(1, 2, 3, 4, 5, 6).dropN(2) should be(List(1, 3, 5))
      }

      it("should return empty list for List(1, 2, 3, 4, 5, 6) after dropping every 1 element") {
        List(1, 2, 3, 4, 5, 6).dropN(1) should be(List())
      }
    }
  }

  describe("My List method implementations") {

    describe("take while") {
      it("should return Nil for empty list") {
        List().myTakeWhile(_ => true) should be(Nil)
      }

      it("should return 1 for List(1) when true") {
        List(1).myTakeWhile(_ => true) should be(List(1))
      }

      it("should return Nil for List(1) when false") {
        List(1).myTakeWhile(_ => false) should be(Nil)
      }

      it("should return List(1) for List(1,2) when taking 1") {
        List(1, 2).myTakeWhile(_ == 1) should be(List(1))
      }

      it("should return List(1) for List(1,2,1) when taking 1") {
        List(1, 2, 1).myTakeWhile(_ == 1) should be(List(1))
      }

      it("should return List(1,1,1) for List(1,1,1,2,1,1) when taking 1") {
        List(1, 1, 1, 2, 1, 1, 1).myTakeWhile(_ == 1) should be(List(1, 1, 1))
      }
    }

    describe("drop while") {
      it("should return Nil for empty list") {
        Nil.myDropWhile(_ => true) should be(Nil)
      }

      it("should return 1 for List(1) when false") {
        List(1).myDropWhile(_ => false) should be(List(1))
      }

      it("should return Nil for List(1) when true") {
        List(1).myDropWhile(_ => true) should be(Nil)
      }

      it("should return List(2) for List(1,2) when taking 1") {
        List(1, 2).myDropWhile(_ == 1) should be(List(2))
      }

      it("should return List(2,1) for List(1,2,1) when taking 1") {
        List(1, 2, 1).myDropWhile(_ == 1) should be(List(2, 1))
      }

      it("should return List(2,1,1) for List(1,1,1,2,1,1) when taking 1") {
        List(1, 1, 1, 2, 1, 1).myDropWhile(_ == 1) should be(List(2, 1, 1))
      }
    }

    describe("map") {
      it("should transform empty list into empty") {
        Nil.myMap(a => a) should be(Nil)
      }

      it("should transform List(2) into List(4) for double function") {
        List(2).myMap(_ * 2) should be(List(4))
      }

      it("should transform List(2,3) into List(4,6) for double function") {
        List(2, 3).myMap(_ * 2) should be(List(4, 6))
      }
    }

    describe("flat map") {

      it("should return Nil for List(Nil)") {
        List(Nil).myFlatMap(a => a) should be(Nil)
      }

      it("should return List(1) for List(List(1))") {
        List(List(1)).myFlatMap(a => a) should be(List(1))
      }

      it("should return List(1, 1) for List(List(1, 1))") {
        List(List(1, 1)).myFlatMap(a => a) should be(List(1, 1))
      }

      it("should return List(1, 1) for List(List(1, 1), Nil)") {
        List(List(1, 1), Nil).myFlatMap(a => a) should be(List(1, 1))
      }

      it("should return List(1, 1, 2, 3, 4) for List(List(1, 1), List(2), List(3, 4))") {
        List(List(1, 1), List(2), List(3, 4)).myFlatMap(a => a) should be(List(1, 1, 2, 3, 4))
      }
    }

    describe("grouped") {

      it("should return Nil for Nil") {
        Nil.myGrouped(1) should be(List(Nil))
      }

      it("should return List(List(1)) for List(1) and group size as 1") {
        List(1).myGrouped(1) should be(List(List(1)))
      }

      it("should return List(List(1), List(2)) for List(1, 2) and group size as 1") {
        List(1, 2).myGrouped(1) should be(List(List(1), List(2)))
      }

      it("should return List(List(1)) for List(1) and group size as 3") {
        List(1).myGrouped(3) should be(List(List(1)))
      }

      it("should return List(List(1, 2)) for List(1, 2) and group size as 2") {
        List(1, 2).myGrouped(2) should be(List(List(1, 2)))
      }

      it("should return List(List(1, 2, 3), List(4)) for List(1, 2, 3, 4) and group size as 3") {
        List(1, 2, 3, 4).myGrouped(3) should be(List(List(1, 2, 3), List(4)))
      }
    }

    describe("filter") {
      it("should return Nil for Nil when filter is satisfied") {
        Nil.myFilter(f => true) should be(Nil)
      }

      it("should return correct when filter is satisfied") {
        List(1, 2, 3).myFilter(_ > 1) should be(List(2, 3))
      }
    }

    describe("zip with index") {
      it("should return Nil for empty list") {
        Nil.myZipWithIndex should be(Nil)
      }

      it("should return zipped list for non-empty list") {
        List(1, 2, 3).myZipWithIndex should be(List((1, 0), (2, 1), (3, 2)))
      }
    }
  }
}