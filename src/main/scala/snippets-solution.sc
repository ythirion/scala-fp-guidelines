// Strive for totality - constrain the input
case class NonZeroInteger private(value: Int) {
  def toInt = value
}

object NonZeroInteger {
  def toNonZeroInteger(value: Int): NonZeroInteger = {
    if (value == 0) throw new IllegalArgumentException("0 is not authorized")
    new NonZeroInteger(value)
  }
}

def twelveDividedBy(n: NonZeroInteger): Int = {
  n.toInt match {
    case 3 => 4
    case 2 => 6
    case 1 => 12
  }
}
twelveDividedBy(NonZeroInteger(2))

// Strive for totality - constrain the input
def twelveDividedBy(n: Int): Option[Int] = {
  n match {
    case 3 => Some(4)
    case 2 => Some(6)
    case 1 => Some(12)
    case _ => None
  }
}
twelveDividedBy(1) // Some(12)
twelveDividedBy(0)

// Parameterize all the things
def transform(initialValue: Int, n: Int, action: (Int, Int) => Int): Int =
  (1 to n).fold(initialValue)(action)

def product(n: Int) = transform(1, n, (product, i) => product * i)
def sum(n: Int) = transform(0, n, (sum, i) => sum + i)

def printList(list: List[Int], print: Int => Unit): Unit = list.foreach(print)
printList(List.range(1, 10), i => println(s"the number is $i"))

// Hollywood principle -> continuations
def divide(top: Int,
           bottom: Int,
           onZero: () => Unit,
           onSuccess: Int => Unit): Unit = {
  bottom match {
    case 0 => onZero()
    case _ => onSuccess(top / bottom)
  }
}

divide(89,
  0,
  () => println("division by 0"),
  result => println(result))

// Chaining callbacks with continuations
case class UserInput(key: Int)

def doSomething(input: UserInput): Option[UserInput] = Some(input.copy(input.key / 2))
def doSomethingElse(input: UserInput): Option[UserInput] = Some(input.copy(input.key / 2))
def doAThirdStuff(input: UserInput): Option[UserInput] = Some(input.copy(input.key / 2))

def uglyRefactored(input: UserInput): Option[UserInput] = {
  doSomething(input)
    .flatMap(doSomethingElse)
    .flatMap(doAThirdStuff)
}
uglyRefactored(UserInput(90))