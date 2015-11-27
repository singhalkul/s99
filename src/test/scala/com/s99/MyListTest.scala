package com.s99

import com.s99.MyList._
import org.scalatest.{FunSpec, Matchers}

/**
 * author: Kul.
 */
class MyListTest extends FunSpec with Matchers {

  describe("Scala 99 problems") {

    describe("01. finding the last element of list") {
      it("should return last element from a list") {
        List(1, 2, 3, 4, 5).myLast() shouldEqual Some(5)
      }

      it("should return None") {
        Nil.myLast() shouldEqual None
      }
    }

    describe("02. finding penultimate element of list") {
      it("should return None if list is empty") {
        Nil.penultimate() shouldEqual None
      }

      it("should return None of list size is 1") {
        List(1).penultimate() shouldEqual None
      }

      it("should return the penultimate element from a list of size of 2 elements") {
        List(2, 3).penultimate() shouldEqual Some(2)
      }

      it("should return the penultimate element from a list of size of 3 elements") {
        List(1, 2, 3).penultimate() shouldEqual Some(2)
      }

      it("should return the penultimate element from a list of size greater than 3 elements") {
        List(1, 2, 3, 4, 5).penultimate() shouldEqual Some(4)
      }
    }

    describe("03. finding kth element of list") {
      it("should return None if list is empty") {
        Nil.kth(1) shouldEqual None
      }

      it("should return the kth for list of size 1") {
        List(1).kth(0) shouldEqual Some(1)
      }

      it("should return the 1st for list of size 2") {
        List(1, 2).kth(0) shouldEqual Some(1)
      }

      it("should return the 2nd for list of size 2") {
        List(1, 2).kth(1) shouldEqual Some(2)
      }

      it("should return the 4th for list of size 6") {
        List(1, 2, 3, 4, 5, 6).kth(3) shouldEqual Some(4)
      }
    }

    describe("04. reversing a list should reverse") {
      it("Nil for empty list") {
        Nil.myReverse() shouldEqual Nil
      }

      it("list of size 1") {
        List(1).myReverse() shouldEqual List(1)
      }

      it("list of size 2") {
        List(1, 2).myReverse() shouldEqual List(2, 1)
      }

      it("list of size greater than 2") {
        List(1, 2, 3, 4, 5).myReverse() shouldEqual List(5, 4, 3, 2, 1)
      }
    }

    describe("05. finding length of list") {
      it("should return 0 for empty list") {
        Nil.myLength() shouldEqual 0
      }

      it("should return 1 for list of size 1") {
        List(1).myLength() shouldEqual 1
      }

      it("should return 2 for list of size 2") {
        List(1, 2).myLength() shouldEqual 2
      }

      it("should return 5 for list of size 5") {
        List(1, 2, 3, 4, 5).myLength() shouldEqual 5
      }
    }

    describe("06. checking whether a list is palindrome") {
      it("should return true for empty list") {
        Nil.isPalindrome shouldEqual (right = true)
      }

      it("should return true for list with one element") {
        List(1).isPalindrome shouldEqual (right = true)
      }

      it("should return false for list 1,2") {
        List(1, 2).isPalindrome shouldEqual (right = false)
      }

      it("should return true for list 1,2,1,2,1") {
        List(1, 2, 1, 2, 1).isPalindrome shouldEqual (right = true)
      }

      it("should return false for list 1,2,3") {
        List(1, 2, 3).isPalindrome shouldEqual (right = false)
      }
    }

    describe("07. flattening a list") {
      it("should return Nil for empty list") {
        Nil.myFlatten() shouldEqual Nil
      }

      it("should return same list for List(1,2,3") {
        List(1, 2, 3).myFlatten() shouldEqual List(1, 2, 3)
      }

      it("should return flattened list for List(1,List(2,3,4),3)") {
        List(1, List(2, 3, 4), 3).myFlatten() shouldEqual List(1, 2, 3, 4, 3)
      }

      it("should return flattened list for List(List(List(2)))") {
        List(List(List(2))).myFlatten() shouldEqual List(2)
      }

      it("should return flattened list for List(List(List(1,2)), 3)") {
        List(List(List(1, 2)), 3).myFlatten() shouldEqual List(1, 2, 3)
      }
    }

    describe("08. removing consecutive duplicates from a list") {
      it("should return Nil from empty list") {
        Nil.compress() shouldEqual Nil
      }

      it("should return List(1) for List(1)") {
        List(1).compress() shouldEqual List(1)
      }

      it("should return List(1) for List(1,1)") {
        List(1, 1).compress() shouldEqual List(1)
      }

      it("should return List(1,2) for List(1,2)") {
        List(1, 2).compress() shouldEqual List(1, 2)
      }

      it("should return compressed list") {
        List(1, 1, 1, 1, 2, 3, 3, 1, 1, 4, 5, 5, 5, 5).compress() shouldEqual List(1, 2, 3, 1, 4, 5)
      }
    }

    describe("09. packing consecutive duplicates") {

      it("should return List of Nil for empty List") {
        Nil.pack() shouldEqual List(Nil)
      }

      it("should return List(List(1)) for List(1)") {
        List(1).pack() shouldEqual List(List(1))
      }

      it("should return List(List(1), List(2)) for List(1,2)") {
        List(1, 2).pack() shouldEqual List(List(1), List(2))
      }

      it("should return List(List(1,1,1), List(2,2), List(1)) for List(1,1,1,2,2,1)") {
        List(1, 1, 1, 2, 2, 1).pack() shouldEqual List(List(1, 1, 1), List(2, 2), List(1))
      }
    }

    describe("10. run length encoding of duplicate elements in a list") {

      it("should return List of Nil for empty list") {
        Nil.runLengthEncoding() shouldEqual Nil
      }

      it("should return List(List((1, 1))) for List(1)") {
        List(1).runLengthEncoding() shouldEqual List((1, 1))
      }

      it("should return List((3, 1), (1, 2), (2, 3)) for List(1,1,1,2,3,3)") {
        List(1, 1, 1, 2, 3, 3).runLengthEncoding() shouldEqual List((3, 1), (1, 2), (2, 3))
      }
    }

    describe("11. modified run length encoding of duplicate elements in a list") {

      it("should return Nil for empty list") {
        Nil.runLengthEncodingModified() shouldEqual Nil
      }

      it("should return List(1) for List(1)") {
        List(1).runLengthEncodingModified() shouldEqual List(1)
      }

      it("should return List((2, 1)) for List(1, 1)") {
        List(1, 1).runLengthEncodingModified() shouldEqual List((2, 1))
      }

      it("should return List((2, 1), 3) for List(1, 1, 3)") {
        List(1, 1, 3).runLengthEncodingModified() shouldEqual List((2, 1), 3)
      }

      it("should return List((3, 1), 2, (2, 3)) for List(1,1,1,2,3,3)") {
        List(1, 1, 1, 2, 3, 3).runLengthEncodingModified() shouldEqual List((3, 1), 2, (2, 3))
      }
    }

    describe("12. Decode a run-length encoded list") {

      def decode[A](t: (Int, A)) = List.fill(t._1)(t._2)

      it("should return List(2) for List((1, 2))") {
        List((1, 2)).myFlatMap(decode) shouldEqual List(2)
      }

      it("should return List(2, 3, 4, 5) for List((1, 2), (2, 3), (2, 5))") {
        List((1, 2), (2, 3), (2, 5)).myFlatMap(decode) shouldEqual List(2, 3, 3, 5, 5)
      }
    }

    describe("13. direct run length encoding (without using pack method from previous example) of duplicate elements in a list") {

      it("should return Nil for empty list") {
        Nil.encodeDirect() shouldEqual Nil
      }

      it("should return List(List((1, 1))) for List(1)") {
        List(1).encodeDirect() shouldEqual List((1, 1))
      }

      it("should return List((3, 1), (1, 2), (2, 3)) for List(1,1,1,2,3,3)") {
        List(1, 1, 1, 2, 3, 3).encodeDirect() shouldEqual List((3, 1), (1, 2), (2, 3))
      }
    }

    describe("14. duplicate the elements of a list") {

      it("should return List(1, 1) for List(1)") {
        List(1).duplicate shouldEqual List(1, 1)
      }

      it("should return List(1, 1, 3, 3) for List(1, 3)") {
        List(1, 3).duplicate shouldEqual List(1, 1, 3, 3)
      }
    }

    describe("15. duplicate the elements of a list given number of times") {

      it("should return List(1, 1) for List(1) duplicated 2 times") {
        List(1).duplicateN(2) shouldEqual List(1, 1)
      }

      it("should return List(1, 1, 1, 3, 3, 3) for List(1, 3) duplicated 3 times") {
        List(1, 3).duplicateN(3) shouldEqual List(1, 1, 1, 3, 3, 3)
      }
    }

    describe("16. Drop every Nth element from a list") {
      it("should return List(1, 3, 5) for List(1, 2, 3, 4, 5, 6) after dropping every 2nd element") {
        List(1, 2, 3, 4, 5, 6).dropN(2) shouldEqual List(1, 3, 5)
      }

      it("should return empty list for List(1, 2, 3, 4, 5, 6) after dropping every 1 element") {
        List(1, 2, 3, 4, 5, 6).dropN(1) shouldEqual List()
      }
    }

    describe("17. Split a list into two parts") {
      it("should return (Nil, Nil) when splitting an empty list") {
        Nil.mySplit(1) shouldEqual (Nil, Nil)
      }

      it("should return (List(1), Nil) when splitting List(1)") {
        List(1).mySplit(1) shouldEqual (List(1), Nil)
      }

      it("should return (List(1), List(2)) when splitting List(1,2) at 1") {
        List(1, 2).mySplit(1) shouldEqual (List(1), List(2))
      }

      it("should return (List(1, 2), Nil) when splitting List(1,2) at 2") {
        List(1, 2).mySplit(2) shouldEqual (List(1, 2), Nil)
      }

      it("should return (Nil, List(1, 2)) when splitting List(1,2) at 0") {
        List(1, 2).mySplit(0) shouldEqual (Nil, List(1, 2))
      }

      it("should return (List(1, 2), List(3,4,5)) when splitting List(1,2,3,4,5) at 2") {
        List(1, 2, 3, 4, 5).mySplit(2) shouldEqual (List(1, 2), List(3,4,5))
      }
    }

    describe("18. Extract a slice from a list") {

      it("should return Nil when extracting slice from 0 to 0 in List(1)") {
        List(1).mySlice(0, 0) shouldEqual Nil
      }

      it("should return List(1) when extracting slice from 0 to 1 in List(1)") {
        List(1).mySlice(0, 1) shouldEqual List(1)
      }

      it("should return List(3, 4, 5) when extracting slice from 3 to 5 in List(0,1,2,3,4,5,6)") {
        List(0,1,2,3,4,5,6).mySlice(3, 6) shouldEqual List(3,4,5)
      }
    }

    describe("19. Rotate a list N places to the left") {

      it("should rotate list by 3 places from beginning when n = 3") {
        List(1,2,3,4,5,6,7,8,9,10).rotateLeft(3) shouldEqual List(4,5,6,7,8,9,10,1,2,3)
      }

      it("should rotate list by 3 places from end when n = -3") {
        List(1,2,3,4,5,6,7,8,9,10).rotateLeft(-3) shouldEqual List(8,9,10,1,2,3,4,5,6,7)
      }
    }

    describe("20. Remove the Kth element from a list") {

      it("should remove 2nd element from the list") {
        List(1,2,3,4).removeKthElement(1) shouldEqual (List(1,3,4), 2)
      }

      it("should remove last element from the list") {
        List(1,2,3,4,5,6,7,8,9).removeKthElement(8) shouldEqual (List(1,2,3,4,5,6,7,8), 9)
      }

      it("should remove first element from the list") {
        List(1,2,3).removeKthElement(0) shouldEqual (List(2,3), 1)
      }
    }

    describe("21. Insert an element at a given position into a list") {

      it("should insert element at 2nd position") {
        List(1,2,3,4).insertAt(1, 5) shouldEqual List(1,5,2,3,4)
      }

      it("should insert element at starting of list") {
        List(1,2,3,4).insertAt(0, 5) shouldEqual List(5,1,2,3,4)
      }

      it("should insert element at end of list") {
        List(1,2,3,4).insertAt(4, 5) shouldEqual List(1,2,3,4,5)
      }
    }

    describe("22. Create a list containing all integers within a given range.") {
      it("should create List(4,5,6,7,8,9) from range 4 to 9") {
        MyList.createListForRange(4, 9) shouldEqual List(4,5,6,7,8,9)
      }

      it("should create List(4,5,6,7,8,9) from range -4 to 4") {
        MyList.createListForRange(-4, 4) shouldEqual List(-4,-3,-2,-1,0,1,2,3,4)
      }
    }

    describe("23. Extract a given number of randomly selected elements from a list") {
      it("should remove 3 elements randomly from a list of 8") {
        List(1,2,3,4,5,6,7,8).randomDelete(3).size shouldEqual 3
      }

      it("should remove 3 elements randomly from a list of 3") {
        List(1,2,3).randomDelete(3).size shouldEqual 3
      }
    }

    describe("24. Lotto: Draw N different random numbers from the set 1..M") {
      it("should extract 6 random elements from range 1 to 21") {
        MyList.lotto(6, 21).size shouldEqual 6
      }
    }
  }

  describe("My List method implementations") {

    describe("take while") {
      it("should return Nil for empty list") {
        List().myTakeWhile(_ => true) shouldEqual Nil
      }

      it("should return 1 for List(1) when true") {
        List(1).myTakeWhile(_ => true) shouldEqual List(1)
      }

      it("should return Nil for List(1) when false") {
        List(1).myTakeWhile(_ => false) shouldEqual Nil
      }

      it("should return List(1) for List(1,2) when taking 1") {
        List(1, 2).myTakeWhile(_ == 1) shouldEqual List(1)
      }

      it("should return List(1) for List(1,2,1) when taking 1") {
        List(1, 2, 1).myTakeWhile(_ == 1) shouldEqual List(1)
      }

      it("should return List(1,1,1) for List(1,1,1,2,1,1) when taking 1") {
        List(1, 1, 1, 2, 1, 1, 1).myTakeWhile(_ == 1) shouldEqual List(1, 1, 1)
      }
    }

    describe("drop while") {
      it("should return Nil for empty list") {
        Nil.myDropWhile(_ => true) shouldEqual Nil
      }

      it("should return 1 for List(1) when false") {
        List(1).myDropWhile(_ => false) shouldEqual List(1)
      }

      it("should return Nil for List(1) when true") {
        List(1).myDropWhile(_ => true) shouldEqual Nil
      }

      it("should return List(2) for List(1,2) when taking 1") {
        List(1, 2).myDropWhile(_ == 1) shouldEqual List(2)
      }

      it("should return List(2,1) for List(1,2,1) when taking 1") {
        List(1, 2, 1).myDropWhile(_ == 1) shouldEqual List(2, 1)
      }

      it("should return List(2,1,1) for List(1,1,1,2,1,1) when taking 1") {
        List(1, 1, 1, 2, 1, 1).myDropWhile(_ == 1) shouldEqual List(2, 1, 1)
      }
    }

    describe("map") {
      it("should transform empty list into empty") {
        Nil.myMap(a => a) shouldEqual Nil
      }

      it("should transform List(2) into List(4) for double function") {
        List(2).myMap(_ * 2) shouldEqual List(4)
      }

      it("should transform List(2,3) into List(4,6) for double function") {
        List(2, 3).myMap(_ * 2) shouldEqual List(4, 6)
      }
    }

    describe("flat map") {

      it("should return Nil for List(Nil)") {
        List(Nil).myFlatMap(a => a) shouldEqual Nil
      }

      it("should return List(1) for List(List(1))") {
        List(List(1)).myFlatMap(a => a) shouldEqual List(1)
      }

      it("should return List(1, 1) for List(List(1, 1))") {
        List(List(1, 1)).myFlatMap(a => a) shouldEqual List(1, 1)
      }

      it("should return List(1, 1) for List(List(1, 1), Nil)") {
        List(List(1, 1), Nil).myFlatMap(a => a) shouldEqual List(1, 1)
      }

      it("should return List(1, 1, 2, 3, 4) for List(List(1, 1), List(2), List(3, 4))") {
        List(List(1, 1), List(2), List(3, 4)).myFlatMap(a => a) shouldEqual List(1, 1, 2, 3, 4)
      }
    }

    describe("grouped") {

      it("should return Nil for Nil") {
        Nil.myGrouped(1) shouldEqual List(Nil)
      }

      it("should return List(List(1)) for List(1) and group size as 1") {
        List(1).myGrouped(1) shouldEqual List(List(1))
      }

      it("should return List(List(1), List(2)) for List(1, 2) and group size as 1") {
        List(1, 2).myGrouped(1) shouldEqual List(List(1), List(2))
      }

      it("should return List(List(1)) for List(1) and group size as 3") {
        List(1).myGrouped(3) shouldEqual List(List(1))
      }

      it("should return List(List(1, 2)) for List(1, 2) and group size as 2") {
        List(1, 2).myGrouped(2) shouldEqual List(List(1, 2))
      }

      it("should return List(List(1, 2, 3), List(4)) for List(1, 2, 3, 4) and group size as 3") {
        List(1, 2, 3, 4).myGrouped(3) shouldEqual List(List(1, 2, 3), List(4))
      }
    }

    describe("filter") {
      it("should return Nil for Nil when filter is satisfied") {
        Nil.myFilter(f => true) shouldEqual Nil
      }

      it("should return correct when filter is satisfied") {
        List(1, 2, 3).myFilter(_ > 1) shouldEqual List(2, 3)
      }
    }

    describe("zip with index") {
      it("should return Nil for empty list") {
        Nil.myZipWithIndex shouldEqual Nil
      }

      it("should return zipped list for non-empty list") {
        List(1, 2, 3).myZipWithIndex shouldEqual List((1, 0), (2, 1), (3, 2))
      }
    }

    describe("take") {
      it("should take 1 element from list of 3 when n = 1") {
        List(1,2,3).myTake(1) shouldEqual List(1)
      }

      it("should take nothing from list of 3 when n = 0") {
        List(1,2,3).myTake(0) shouldEqual Nil
      }

      it("should take nothing from list of 3 when n < 0") {
        List(1,2,3).myTake(-1) shouldEqual Nil
      }

      it("should take all elements from list of 3 when n = 3") {
        List(1,2,3).myTake(3) shouldEqual List(1, 2, 3)
      }

      it("should take all elements from list of 3 when n > 3") {
        List(1,2,3).myTake(5) shouldEqual List(1, 2, 3)
      }
    }

    describe("drop") {
      it("should drop 1 element from list of 3 when n = 1") {
        List(1,2,3).myDrop(1) shouldEqual List(2, 3)
      }

      it("should drop nothing from list of 3 when n = 0") {
        List(1,2,3).myDrop(0) shouldEqual List(1,2,3)
      }

      it("should drop nothing from list of 3 when n < 0") {
        List(1,2,3).myDrop(-1) shouldEqual List(1,2,3)
      }

      it("should drop all elements from list of 3 when n = 3") {
        List(1,2,3).myDrop(3) shouldEqual Nil
      }

      it("should drop all elements from list of 3 when n > 3") {
        List(1,2,3).myDrop(5) shouldEqual Nil
      }
    }
  }
}