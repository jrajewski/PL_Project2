val chinese: List[String] = List("ling", "yi", "er", "san", "si", "wu", "liu", "qi", "ba", "jiu", "shi")
val english: List[String] = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
val numerals: List[Int] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

//var translated: List[Int] = Nil

def member(element: String, aList: List[String]): Boolean = {
  aList match {
    case Nil => false
    case listHead :: listTail => if (element == listHead) true else member(element, listTail)
  }
}

//testing member class
member("zero", english)
member("ling", chinese)
member("zero", chinese)

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
  val toCalculate: List[String] = englishElements ::: chineseElements
  val toWork: List[Int] = Nil

  val translated: List[Int] = translation(toCalculate, toWork)

  if (translated.isEmpty) print("You failed, again.")
}

go(List("zero", "one"))

def translation(input: List[String], work: List[Int]): List[Int] = {
  //val listHead: String = input.head

  val inputEmpty: Boolean = input.isEmpty
  inputEmpty match {
    case false =>
      input.head match {
        case "ling" | "zero" => translation(input.tail, 0 :: work)
        case "yi" | "one" => translation(input.tail, 1 :: work)
        case "er" | "two" => translation(input.tail, 2 :: work)
        case "san" | "three" => translation(input.tail, 3 :: work)
        case "si" | "four" => translation(input.tail, 4 :: work)
        case "wu" | "five" => translation(input.tail, 5 :: work)
        case "liu" | "six" => translation(input.tail, 6 :: work)
        case "qi" | "seven" => translation(input.tail, 7 :: work)
        case "ba" | "eight" => translation(input.tail, 8 :: work)
        case "jiu" | "nine" => translation(input.tail, 9 :: work)
        case "shi" | "ten" => translation(input.tail, 10 :: work)
      }
      if (work.isEmpty == false) work.foreach(println(_))
      return work
    case true => work
  }
}