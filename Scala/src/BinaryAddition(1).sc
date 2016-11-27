// COSC 455 - Programming Languages: Implementation and Design
// Project 2

// NAME: <Your name here>


// Test Cases
val pTest1: List[Int] = List (1, 1, 1, 1, 0)
val qTest1: List[Int] = List(1, 0, 1, 1)
val test1ExectedSolution: List[Int] = List(1, 0, 1, 0, 0, 1)

val pTest2: List[Int] = List (1, 0, 0, 1, 1, 0, 1)
val qTest2: List[Int] = List(1, 0, 0, 1, 0)
val test2ExectedSolution: List[Int] = List(1, 0, 1, 1, 1, 1, 1)

val pTest3: List[Int] = List (1, 0, 0, 1, 0, 0, 1)
val qTest3: List[Int] = List(1, 1, 0, 0, 1)
val test3ExectedSolution: List[Int] = List(1, 1, 0, 0, 0, 1, 0)

val pTest4: List[Int] = List (1, 0, 0, 0, 1, 1, 1)
val qTest4: List[Int] = List(1, 0, 1, 1, 0)
val test4ExectedSolution: List[Int] = List(1, 0, 1, 1, 1, 0, 1)

// This function does the binary addition when there are uneven lists and still must
// finish the add with the carry bits.
def finishBinaryAdd(remainingBits: List[Boolean], carryBit: Boolean): List[Boolean] = {
  val hasBits = remainingBits.isEmpty
  hasBits match{
    case true =>
      carryBit match{
        case true =>  //most logic here
        case false => return remainingBits
      }
    case false =>
      carryBit match{
        case true => return List{true}
      }
}

// This function determines what the next carry bit should be based on current bits.
def getNextCarryBit(pBit: Boolean, qBit: Boolean, carrybit: Boolean): Boolean = {

}

// This function does the binary addition of two Booleans and a carry bit.
def addBits(pBit: Boolean, qBit: Boolean, carryBit: Boolean): Boolean = {
  var addBooleanList: List[Boolean] = {pBit, qBit, carryBit}
  var addIntList: List[Int] = convertBooleanListToIntList(addBooleanList)
  var sum: Int = addIntList.fold(0){ (x,y) => x + y}
  sum match{
    case 0 => return false
    case 1 => return true
    case 2 => return false
    case 3 => return true
  }
}

// This function does the binary addition of two boolean lists. Note that the lists may not be equal in length.
def doBinaryAddition(pBits: List[Boolean], qBits: List[Boolean], carryBit: Boolean): List[Boolean] = {
  val pList: List[Int] = convertBooleanListToIntList(pBits)
  val qList: List[Int] = convertBooleanListToIntList(qBits)





}

// This function converts a binary integer list into its corresponding boolean list.
def convertIntListToBooleanList(intList: List[Int]): List[Boolean] = intList.map {
  case 1 => true
  case 0 => false
}

// This function converts a boolean list into its corresponding binary integer list.
def convertBooleanListToIntList(booleanList: List[Boolean]): List[Int] = booleanList.map{
  case true => 1
  case false => 0
}

/* This is the "main" function to do binary addition. This function should:
    1. Convert the input parameter lists from integers to boolean
    2. Reverse the lists (since binary addition is performed right to left)
    3. Perform the binary addition with the doBinaryAddition function
    4. Reverse the lists (to get back in proper order)
    5. Convert the answer back to binary integer form for output
  Note that the initial carry bit is assumed to be 0 (i.e., false).
*/
def binaryAddition(pList: List[Int], qList: List[Int]): List[Int] = {
  convertBooleanListToIntList(reverseList(doBinaryAddition(convertIntListToBooleanList(pList).reverse, convertIntListToBooleanList(qList).reverse, getNextCarryBit(pList.head, qList.head, false))))

  convertIntListToBooleanList(pList)
  convertIntListToBooleanList(qList)
  reverseList(convertIntListToBooleanList(pList))
  reverseList(convertIntListToBooleanList(qList))

  //this irritates me
  doBinaryAddition(convertIntListToBooleanList(pList).reverse, convertIntListToBooleanList(qList).reverse, getNextCarryBit())
}


// Testing binary addition.
if (binaryAddition(pTest1, qTest1).equals(test1ExectedSolution)) println("Test 1 passes!") else println("Test 1 fails.")
if (binaryAddition(pTest2, qTest2).equals(test2ExectedSolution)) println("Test 2 passes!") else println("Test 2 fails.")
if (binaryAddition(pTest3, qTest3).equals(test3ExectedSolution)) println("Test 3 passes!") else println("Test 3 fails.")
if (binaryAddition(pTest4, qTest4).equals(test4ExectedSolution)) println("Test 4 passes!") else println("Test 4 fails.")

// Extra Credit workspace
