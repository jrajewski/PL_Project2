val chinese: List[String] = List("ling", "yi", "er", "san", "si", "wu", "liu", "qi", "ba", "jiu", "shi")
val english: List[String] = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")

def member(element: String, aList: List[String]): Boolean = {
  aList match {
    case Nil => false
    case listHead :: listTail => if (element == listHead) true else member(element, listTail)
  }
}

//testing member class
/*member("zero", english)
member("ling", chinese)
member("zero", chinese)
*/

def intersection(aList: List[String], anotherList: List[String]): List[String] = {
  anotherList match {
    case Nil => Nil
    case anotherListHead :: anotherListTail =>
      if (member(anotherListHead, aList)) anotherListHead :: intersection(aList, anotherListTail)
      else intersection(aList, anotherListTail)
  }
}

def go(calc: List[String]): Unit = {
  //check for valid elements in each language
  val englishElements: List[String] = intersection(calc, english)
  val chineseElements: List[String] = intersection(calc, chinese)

  //merge the 2 lists
  val toTranslate: List[String] = englishElements ::: chineseElements

  val translated: List[Int] = translation(toTranslate)

  print("Translation: ")
  translated.foreach(x => print(x + " "))
  println()

  //conditional system used to print the correct icon until the last element before printing the sum/product
  print("Addition: ")
  val end = translated.length
  var count: Int = 0
  val sum: Int = addition(translated)
  translated.foreach(x => if(count < end - 1){ print(x +  " + "); count+=1} else print(x + " "))
  println("= " + addition(translated))

  print("Multiplication: ")
  count = 0
  val product: Int = multiplication(translated)
  translated.foreach(x => if(count < end - 1){ print(x +  " * "); count+=1} else print(x + " "))
  println("= " + multiplication(translated))

}

//Test cases
go(List("zero", "one", "joe", "san", "three"))
go(List("one", "er", "three", "si", "error"))

def translation(input: List[String]): List[Int] = {
  input.map {
      case "ling" | "zero" => 0//translation(input.tail, 0 :: work)
      case "yi" | "one" => 1 //translation(input.tail, 1 :: work)
      case "er" | "two" => 2// translation(input.tail, 2 :: work)
      case "san" | "three" => 3 //translation(input.tail, 3 :: work)
      case "si" | "four" => 4//translation(input.tail, 4 :: work)
      case "wu" | "five" => 5//translation(input.tail, 5 :: work)
      case "liu" | "six" => 6//translation(input.tail, 6 :: work)
      case "qi" | "seven" => 7//translation(input.tail, 7 :: work)
      case "ba" | "eight" => 8//translation(input.tail, 8 :: work)
      case "jiu" | "nine" => 9//translation(input.tail, 9 :: work)
      case "shi" | "ten" => 10//translation(input.tail, 10 :: work)
  }
}

def addition(input: List[Int]): Int = {
  input match {
    //if there are no more elements remaining, just pass back a zero in the recursion so no more is added
    case n :: rest => n + addition(rest)
    case Nil => 0
  }
}

def multiplication(input: List[Int]): Int = {
  input match {
    //if there are no more remaining elements, pass back a 1 because anything multiplied by one
    //will not change the product
    case n :: rest => n * multiplication(rest)
    case Nil => 1
  }
}