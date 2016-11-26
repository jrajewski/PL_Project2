def prime(n: Int): Boolean ={
  val aList: List[Int] = List.range(2, n)
  aList.forall(o => (n % o) != 0)
}

prime(31)

def twinPrime(x: Int, y: Int): Boolean ={
  val z = x - y
  var first, second: Boolean = false
  //var second: Boolean = false

  if (z == 2 || z == -2){
    first = prime(x)
    second = prime(y)
  }

  first && second
}

twinPrime(41, 43)
twinPrime(29, 31)

def twinPrimesList(n: Int): List[Int] = {
  var aList: List[Int] = List.range(3,n)
  var bList: List[Int] = List.range(3,n)
  //bList.filter(o => prime(o))

  aList = aList.filter(p => twinPrime(p, p - 2))
  bList = bList.filter(o => twinPrime(o, o + 2))

  val cList: List[Int] = aList ::: bList
  cList.sortWith(_ < _)
}

twinPrimesList(50)


def goldbach(n: Int): Unit = {
  if(n < 2) System.exit(0)

  var aList: List[Int] = List.range(1, n)
  aList = aList.filter(q => prime(q))
  aList.forall(r => foreach(r + _))
  aList
  /*
The Goldbach Conjecture states
that every positive even number greater than 2 is the sum of two prim numbers.
For example, 28 = 5 + 23. Your function is to find the two prime numbers that
sum up to a given even integer and print the composition. For example goldbach(28)
would print 5 + 23 = 28. You should provide error checking to make sure the integer
 parameter is even and greater than 2.

 */


}