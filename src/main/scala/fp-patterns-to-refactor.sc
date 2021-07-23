def product(n: Int) = {
  var product = 1
  for (i <- 1 to n)
    product *= i
  product
}

def sum(n: Int) = {
  var sum = 0
  for (i <- 1 to n)
    sum += i
  sum
}

product(10)
sum(10)

trait BunchOfStuff {
  def doSomething(x: Int) : Int
}

type bunchOfStuff = Int => Int

def add2(x: Int) = x + 2
def times3(x: Int) = x * 3

val bunch: bunchOfStuff = add2
bunch(9)

