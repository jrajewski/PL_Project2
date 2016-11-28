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


def goldbach(n: Int): Unit = {
  if(n < 2) System.exit(0)

  var aList: List[Int] = List.range(1, n)
  aList = aList.filter(q => prime(q))

  //not working
  //aList.forall(r => foreach(r + _))


}

goldbach(50)