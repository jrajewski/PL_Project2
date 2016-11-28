def prime(n: Int): Boolean ={
  val aList: List[Int] = List.range(2, n)
  aList.forall(o => (n % o) != 0)
}

prime(31)

def twinPrime(x: Int, y: Int): Boolean ={
  val z = x - y
  var first, second: Boolean = false
  //var second: Boolean = false

  //only checks for primality if the numbers differ by 2
  //if they don't then there is no need to continue calculation
  if (z == 2 || z == -2){
    first = prime(x)
    second = prime(y)
  }

  //return if both elements passed are prime
  //false by default
  first && second
}

//2 test cases for twinprime
twinPrime(41, 43)
twinPrime(29, 31)

def twinPrimesList(n: Int): List[Int] = {
  //in my solution, two lists are required. see comments below
  var aList: List[Int] = List.range(3,n)
  var bList: List[Int] = List.range(3,n)
  //bList.filter(o => prime(o))

  //created two lists with twinPrime function, one to check two integers behind the current elements
  //and another list to check two integers ahead of the current element
  aList = aList.filter(p => twinPrime(p, p - 2))
  bList = bList.filter(o => twinPrime(o, o + 2))

  //combine both sets of twin prime lists
  var cList: List[Int] = aList ::: bList

  //sorts combined twinprimes list least to greatest
  cList = cList.sortWith(_ < _)
  cList.distinct
}

twinPrimesList(50)

//Old attempt to use prime filtering. Thought would be efficient, but could not
//determine an efficient solution, let alone a working solution

/*
def goldbachHelp(y: Int, element: Int, listTail: Int, primeList: List[Int]): Unit = {
  //declared as var because could not reassign to val
  var tempTail: Int = listTail
  var tempElement: Int = element
  //println("tempElement = " + tempElement)
  //println("tempTail = " + tempTail)
  //println()
  (primeList(element) + primeList(listTail) == y) match{
    case true => println(primeList(element) + " + " + primeList(listTail) + " = " + y)
      System.exit(0)
    case false =>
      tempTail -= 1
      tempTail >= 0 match{
        case true =>
          tempElement < primeList.length match{
            case true =>
              goldbachHelp(y, tempElement, tempTail, primeList)
              tempTail match{
                  //resets tail value
                case 0 => tempTail = primeList.length - 1
                case _ => Nil
              }

            case false => println("You did something wrong, Joe.")
          }
        case false =>

      }
      //goldbachHelp(y, tempElement, tempTail, primeList)


  }
}


def goldbach(x: Int): Unit = {
  val continue = (x % 2 == 0) && (x > 2)
  continue match{
    case true =>
      var aList: List[Int] = List.range(1, x)
      aList = aList.filter(q => prime(q))
      println(aList)
      println(aList.length)
      goldbachHelp(x, 0, (aList.length - 1), aList)
    case false =>
      println("Value must be even and greater than 2 for Goldbach's Conjecture.")
  }
}

goldbach(15)
goldbach(16)
//goldbach(50)
*/


def goldbach(x: Int): Unit ={
  val continue = (x % 2 == 0) && (x > 2)
  continue match{
    case true=>
      //send 1 and x - 1 to start looking for combinations that add up to x
      //skip 0 and x because neither are prime
      var a = 1; var b = x - 1
      goldbachAssist(a, b, x )
    case false =>
      println("Value must be even and greater than 2 for Goldbach's Conjecture.")
  }
}

def goldbachAssist(a: Int, b: Int, x: Int): Unit = {
  val bothPrime = prime(a) && prime(b)
  bothPrime match{
    case true =>
      //determines whether a sum of 2 primes is the value submitted to goldbach
      val done = (a + b == x)
      done match{
        case true => println(a + " + " + b + " = " + x)
        //no need for false case. a and b will always add up to x in done because
        //every combination from 1 - x-1 is scanned
      }
  //increase the lower bound by 1 and decrease the upper bound by 1
  //ensures that only combinations that add up to x will be selected
    case false => val c = a + 1
      val d = b - 1
      goldbachAssist(c, d, x)
  }
}

//Test cases
goldbach(50)
goldbach(48)
goldbach(47)
goldbach(10)
goldbach(18)
goldbach(38)
goldbach(22)
goldbach(1000)