/*

This program is not a part of my solution. I added it to the project so
that I could quickly refer to it for understanding of high level
functional programming in Scala.

Just FYI in case this accidentally ends up in my final commit.

-Joe Rajewski

*/

val testList: List[Int] = List(1, 3, 6, 8, 10)

def exercise1(aList : List[Int]) : List[Int] =
  aList.map(listElement => (listElement * 3) + 1)

// test exercise1
exercise1(testList)

def triplePlus1(anInt : Int) : Int = (anInt * 3) + 1

// test triplePlus1
triplePlus1(5)

def exercise2(aList : List[Int]) : List[Int] =
  aList.map(triplePlus1)

// test exercise2
exercise2(testList)

def exercise3(aList : List[Int]) : List[Int] =
  aList
    .filter(_ > 4 )
    .map(triplePlus1)

// test exercise3
exercise3(testList)

def exercise4(aList : List[Int]) : Int  =
  aList
    .filter(_ > 4 )
    .map(triplePlus1)
    .foldLeft(0)(_ + _)

// test exercise4
exercise4(testList)

// factorial using pattern matching
def fact(n: Int): Int =
n match {
  case 0 => 1
  case _ => n * fact(n-1)
}

// test fact
fact(5)

// myMember
def myMember(element: Int, aList: List[Int]): Boolean =
aList match {
  case Nil => false
  case listHead::listTail => if (element == listHead) true else myMember(element, listTail)
}

// test myMember
myMember(3, List(4, 5, 2, 3))

// myUnion
def myUnion(aList: List[Int], anotherList: List[Int]): List[Int] =
anotherList match {
  case Nil => aList
  case anotherListHead::anotherListTail =>
    if (myMember(anotherListHead, aList)) myUnion(aList, anotherListTail)
    else anotherListHead::myUnion(aList, anotherListTail)
}

// test myUnion
val list1: List[Int] = List(1, 2, 4, 5, 6, 10)
val list2: List[Int] = List(10, 8, 4, 3, 7)
myUnion(list1, list2)

def myIntersection(aList: List[Int], anotherList: List[Int]): List[Int] =
  anotherList match {
    case Nil => Nil
    case anotherListHead::anotherListTail =>
      if (myMember(anotherListHead, aList)) anotherListHead::myIntersection(aList, anotherListTail)
      else myIntersection(aList, anotherListTail)
  }

// test myIntersection
myIntersection(list1, list2)

// mySortedUnion
def mySortedUnion(aList: List[Int], anotherList: List[Int]): List[Int] =
myUnion(aList, anotherList).sortWith(_<_)

// mySortedIntersection
def mySortedIntersection(aList: List[Int], anotherList: List[Int]): List[Int] =
myIntersection(aList, anotherList).sortWith(_<_)

// test mySortedUnion & mySortedIntersection
mySortedUnion(list1, list2)
mySortedIntersection(list1, list2)

// il2fl
def il2fl(aList: List[Int]) = aList.map(e => e.toFloat)

// test il2fl
il2fl(list1)

// squarelist
def squarelist(aList: List[Int]) = aList.map(e => e * e)

// test squarelist
squarelist(list2)

// bor & band
// Note that in this case, foldLeft, foldRight or simply fold can be used
def bor(aList: List[Boolean]): Boolean = aList.foldLeft(false)(_||_)
def band(aList: List[Boolean]): Boolean = aList.foldLeft(true)(_&&_)

// test bor and band
val boolList1: List[Boolean] = List(true, false, false, true)
val boolList2: List[Boolean] = List(false, false, false, false)
val boolList3: List[Boolean] = List(true, true, true, true)
bor(boolList1)
bor(boolList2)
bor(boolList3)

band(boolList1)
band(boolList2)
band(boolList3)

// evens
def evens(aList: List[Int]) = aList.filter(a => a % 2 == 0)

def convert(aList: List[Boolean]): List[Int] = aList.map {
  case false => 0
  case true => 1
}

// test convert
convert(boolList1)




