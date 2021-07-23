//Functions as things
def z = 1
def add(x: Int, y: Int): Int = x + y

def add(x: Int) = (y: Int) => x + y
def useFn(f: Int => Int) = f(1) + 2
def transformInt(f: Int => Int, x: Int) = f(x) + 1

useFn(x=> x + 1)
transformInt(x => x + 9, 2)


def printList(list: List[Int], print: Int => Unit): Unit =
  list.foreach(print)

printList(List.range(1, 10), i => println(s"the number is $i"))

def transform(initialValue: Int, n: Int, action: (Int, Int) => Int) =
  (1 to n).fold(initialValue)(action)

def product(n: Int) = transform(1, n, (product, i) => product * i)
def sum(n: Int) = transform(0, n, (sum, i) => sum + i)

product(10)
sum(10)

def product1(n: Int) = {
  var product = 1
  for (i <- 1 to n)
    product *= i
  product
}

def sum1(n: Int) = {
  var sum = 0
  for (i <- 1 to n)
    sum += i
  sum
}

product1(10)
sum1(10)


def doSomethingWithStuff(strategy: Int => Int, x: Int) = strategy(x)

def logger(f: Int => Boolean, x: Int): Boolean = {
  println(s"Input = $x")
  val output = f(x)
  println(s"Output = $output")

  output
}

def isEven(x: Int) = x %2 == 0
def log[T](x: T): T = {
  println(x)
  x
}
def isEvenWithLogging(x: Int) = log(isEven(log(x)))

isEvenWithLogging(11)