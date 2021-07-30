// Functions as things snippets
def z = 1
def add(x: Int, y: Int): Int = x + y

def add(x: Int) = (y: Int) => x + y
def useFn(f: Int => Int) = f(1) + 2
def transformInt(f: Int => Int, x: Int) = f(x) + 1

useFn(x => x + 1)
transformInt(x => x + 9, 2)

// Strive for Totality
def twelveDividedBy(n: Int): Int = {
  n match {
    case 3 => 4
    case 2 => 6
    case 1 => 12
    case 0 => ???
  }
}

// Parameterize all the things
def printList(): Unit = List.range(1, 10).foreach(i => println(s"the number is $i"))
printList()

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

// Function types are "interfaces"
trait BunchOfStuff {
  def doSomething(x: Int): Int
}

type bunchOfStuff = Int => Int

def add2(x: Int) = x + 2
def times3(x: Int) = x * 3

val bunch: bunchOfStuff = add2
bunch(9)

// Functional Strategy pattern
def doSomethingWithStuff(strategy: Int => Int, x: Int) = strategy(x)

// Functional Decorator pattern - using function parameter
def isEven(x: Int) = x % 2 == 0

def logger(f: Int => Boolean, x: Int): Boolean = {
  println(s"Input = $x")
  val output = f(x)
  println(s"Output = $output")

  output
}
def isEvenWithLogging(x: Int) = logger(isEven, x)
isEvenWithLogging(2)

// Functional Decorator pattern - using function composition
def log[T](x: T): T = {
  println(x)
  x
}

def isEvenWithLogging(x: Int) = log(isEven(log(x)))
isEvenWithLogging(11)

// Use partial application for dependency injection
class Customer {}

class Connection {}

def getCustomerFromDatabase(connection: Connection)(customerId: Int): Customer = new Customer()
val connection = new Connection()
def getCustomer(customerId: Int): Customer = getCustomerFromDatabase(connection)(customerId)
getCustomer(9)

// Hollywood principle
def divide(top: Int, bottom: Int) = {
  bottom match {
    case 0 => throw new IllegalArgumentException("division by 0")
    case _ => top / bottom
  }
}
divide(89, 2)

// Chaining callbacks with continuations
case class UserInput(key: Int)

def doSomething(input: UserInput): Option[UserInput] = Some(input.copy(input.key / 2))
def doSomethingElse(input: UserInput): Option[UserInput] = Some(input.copy(input.key / 2))
def doAThirdStuff(input: UserInput): Option[UserInput] = Some(input.copy(input.key / 2))

def uglyFunction(input: UserInput): Option[UserInput] = {
  val x = doSomething(input)
  if (x.isDefined) {
    val y = doSomethingElse(x.get)
    if (y.isDefined) {
      val z = doAThirdStuff(y.get)
      if (z.isDefined) {
        val result = z.get
        Some(result)
      }
      else None
    }
    else None
  }
  else None
}
uglyFunction(UserInput(90))