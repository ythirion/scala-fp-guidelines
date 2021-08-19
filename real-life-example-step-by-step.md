Identify code smells and potential bugs

```scala
class IdCard(val number: Int,val country: Option[String])
abstract class Person(val name: String, val idCard: Option[IdCard])
case class YoungStudent(override val name: String, override val idCard: Option[IdCard]) extends Person(name, idCard)
case class OldStudent(override val name: String, override val idCard: Option[IdCard]) extends Person(name, idCard)

// Boost Students notes for some countries ;-)
val countryWeighting = Map("FR" -> 2.1, "EN" -> 0.8)

// Pourquoi passer 1 Person plutôt qu'1 Student ?
def calculateStudentRank(person: Option[Person], note: Int): Option[Char] = {
  // Pas explicite que la fonction va nous cracher 1 exception -> retourner Option ou Try si besoin
  if(person.isEmpty)
    throw new IllegalArgumentException("Person is not defined")

  // Casse la loi de Demetter
  // Favoriser le mapping ici
  // Pourquoi récupérer la country alors que c'est utile uniquement pour les YoungStudent ?
  val country = if (person.isDefined && person.get.idCard.isDefined) person.get.idCard.get.country else ""

  if(country != ""){
	  // On a déjà vérifier que person pas empty
    // bien d'utiliser le pattern matching pourquoi pas à la place du check person.isEmpty ?
    person match {
      case Some(p) =>
        p match {
          // name / idCard / other not used
          case OldStudent(name, idCard) =>
            note match {
              // Logique de ranking dupliquée
              case it if it >= 0 && it < 5 => Some('D')
              case it if it >= 5 && it < 10 => Some('C')
              case it if it >= 10 && it < 14 => Some('B')
              // BUG : si 1 etudiant a note >= 14 && note  < 15
              case it if it >= 15 && it < 20 => Some('A')
              // Pourquoi lancer 1 exception alors que l'appelant attend 1 Option ?
              case _ => throw new IllegalArgumentException("Note not in range")
            }
          case YoungStudent(name, idCard) =>
          	// get n'est pas safe -> crash si country pas dans la liste
            val weighting = countryWeighting(person.get.idCard.get.country.get)
            // apply weighting for young students
          	// BUG : avec weighting on peut avoir des notes > 20
            note * weighting match {
              case it if it >= 0 && it < 5 => Some('D')
              case it if it >= 5 && it < 10 => Some('C')
              case it if it >= 10 && it < 14 => Some('B')
              case it if it >= 15 && it < 20 => Some('A')
              case _ => throw new IllegalArgumentException("Note not in range")
            }
          case other => throw new IllegalArgumentException("Not a student")
        }
      case None => None
    }

  } else None
}
// Code trop complexe -> if / else + 2 pattern matching

val person = Some { YoungStudent("Peter Griffin", Option(new IdCard(9, Option("FR")))) }
calculateStudentRank(person, 10)
```



1) Replace Person by Student

```scala
class IdCard(val number: Int,val country: Option[String])

abstract class Student(val name: String, val idCard: Option[IdCard])
case class YoungStudent(override val name: String, override val idCard: Option[IdCard]) extends Student(name, idCard)
case class OldStudent(override val name: String, override val idCard: Option[IdCard]) extends Student(name, idCard)

// Boost Students notes for some countries ;-)
val countryWeighting = Map("FR" -> 1.1, "EN" -> 0.8)

def calculateStudentRank(student: Option[Student], note: Int): Option[Char] = { ...
```



2) Extract CalculateRank function

```scala
class IdCard(val number: Int,val country: Option[String])

abstract class Student(val name: String, val idCard: Option[IdCard])
case class YoungStudent(override val name: String, override val idCard: Option[IdCard]) extends Student(name, idCard)
case class OldStudent(override val name: String, override val idCard: Option[IdCard]) extends Student(name, idCard)

// Boost Students notes for some countries ;-)
val countryWeighting = Map("FR" -> 1.1, "EN" -> 0.8)

def calculateRank(note: Int) = {
  note match {
    case it if it >= 0 && it < 5 => Some('D')
    case it if it >= 5 && it < 10 => Some('C')
    case it if it >= 10 && it < 14 => Some('B')
    case it if it >= 15 && it < 20 => Some('A')
    case _ => throw new IllegalArgumentException("Note not in range")
  }
}
def calculateStudentRank(student: Option[Student], note: Int): Option[Char] = {
  if(student.isEmpty)
    throw new IllegalArgumentException("Person is not defined")

  val country = if (student.isDefined && student.get.idCard.isDefined) student.get.idCard.get.country else ""

  if(country != ""){
    student match {
      case Some(p) =>
        p match {
          case OldStudent(name, idCard) =>
            calculateRank(note)
          case YoungStudent(name, idCard) =>
            val weighting = countryWeighting(student.get.idCard.get.country.get)
            // apply weighting for young students
            note * weighting match {
              case it if it >= 0 && it < 5 => Some('D')
              case it if it >= 5 && it < 10 => Some('C')
              case it if it >= 10 && it < 14 => Some('B')
              case it if it >= 15 && it < 20 => Some('A')
              case _ => throw new IllegalArgumentException("Note not in range")
            }
          case other => throw new IllegalArgumentException("Not a student")
        }
      case None => None
    }

  } else None
}

val student = Some { YoungStudent("Peter Griffin", Option(new IdCard(9, Option("FR")))) }
calculateStudentRank(student, 10)
```

3) Mutualize call to new function

```scala
def calculateStudentRank(student: Option[Student], note: Int): Option[Char] = {
  if(student.isEmpty)
    throw new IllegalArgumentException("Person is not defined")

  val country = if (student.isDefined && student.get.idCard.isDefined) student.get.idCard.get.country else ""

  if(country != ""){
    student match {
      case Some(p) =>
        p match {
          case OldStudent(name, idCard) =>
            calculateRank(note)
          case YoungStudent(name, idCard) =>
            val weighting = countryWeighting(student.get.idCard.get.country.get)
            calculateRank(note, weighting)
          case other => throw new IllegalArgumentException("Not a student")
        }
      case None => None
    }

  } else None
}
```

4) Fix bug in calulate Rank

```scala
def calculateRank(note: Int, weighting: Double = 1): Option[Char] = {
  note * weighting match {
    case it if it >= 0 && it < 5 => Some('D')
    case it if it >= 5 && it < 10 => Some('C')
    case it if it >= 10 && it < 14 => Some('B')
    case it if it >= 14 && it <= 20 => Some('A')
    case _ => None
  }
}
```

5) Extract weight note to handle note > 20

```scala
val maxNote = 20
def weightNote(note: Int, weighting: Double): Double = {
  val weightNote = (note * weighting).round
  if (weightNote > maxNote) maxNote else weightNote
}

def calculateRank(note: Int, weighting: Double = 1): Option[Char] = {
  weightNote(note, weighting) match {
    case it if it >= 0 && it < 5 => Some('D')
    case it if it >= 5 && it < 10 => Some('C')
    case it if it >= 10 && it < 14 => Some('B')
    case it if it >= 14 && it <= 20 => Some('A')
    case _ => None
  }
}
```

6) Remove exception from `calculateStudentRank`

```scala
def calculateStudentRank(student: Option[Student], note: Int): Option[Char] = {
  val country = if (student.isDefined && student.get.idCard.isDefined) student.get.idCard.get.country else ""

  if(country != ""){
    student match {
      case Some(p) =>
        p match {
          case OldStudent(name, idCard) =>
            calculateRank(note)
          case YoungStudent(name, idCard) =>
            val weighting = countryWeighting(student.get.idCard.get.country.get)
            calculateRank(note, weighting)
          case other => throw new IllegalArgumentException("Not a student")
        }
      case None => None
    }

  } else None
}
```

7) Remove useless country part

```scala
def calculateStudentRank(student: Option[Student], note: Int): Option[Char] = {
    student match {
      case Some(p) =>
        p match {
          case OldStudent(name, idCard) =>
            calculateRank(note)
          case YoungStudent(name, idCard) =>
            val weighting = countryWeighting(student.get.idCard.get.country.get)
            calculateRank(note, weighting)
          case other => throw new IllegalArgumentException("Not a student")
        }
      case None => None
    }
}
```

8) Extract and make it safe countryWeighting

```scala
def getWeighting(student: Student): Double = {
  def retrieveWeight(s: YoungStudent) = {
    val country = s.idCard.flatMap(card => card.country).getOrElse("")
    countryWeighting.getOrElse(country, defaultWeight)
  }

  student match {
    case s: YoungStudent => retrieveWeight(s)
    case _ => defaultWeight
  }
}
```

9) Use map instead of pattern matching

```scala
val rankByNotes: Map[Range, Char] = Map(
  (0 until 5) -> 'D',
  (5 until  10) -> 'C',
  (10 until  14) -> 'B',
  (14 to 20) -> 'A'
)

def calculateRank(note: Int, weighting: Double): Option[Char] = {
  val weightedNote = weightNote(note, weighting)

  rankByNotes
    .find(kvp => kvp._1.contains(weightedNote))
    .map(kvp => kvp._2)
}
```

10) Putting all together

```scala
class IdCard(val number: Int,val country: Option[String])

abstract class Student(val name: String, val idCard: Option[IdCard])
case class YoungStudent(override val name: String, override val idCard: Option[IdCard]) extends Student(name, idCard)
case class OldStudent(override val name: String, override val idCard: Option[IdCard]) extends Student(name, idCard)

// Boost Students notes for some countries ;-)
val countryWeighting = Map("FR" -> 1.1, "EN" -> 0.8)

val rankByNotes: Map[Range, Char] = Map(
  (0 until 5) -> 'D',
  (5 until  10) -> 'C',
  (10 until  14) -> 'B',
  (14 to 20) -> 'A'
)

val maxNote = 20
val defaultWeight = 1

def weightNote(note: Int, weighting: Double): Double = {
  val weightNote = (note * weighting).round
  if (weightNote > maxNote) maxNote else weightNote
}

def calculateRank(note: Int, weighting: Double): Option[Char] = {
  val weightedNote = weightNote(note, weighting)

  rankByNotes
    .find(kvp => kvp._1.contains(weightedNote))
    .map(kvp => kvp._2)
}

def getWeighting(student: Student): Double = {
  def retrieveWeight(s: YoungStudent): Double = {
    val country = s.idCard.flatMap(card => card.country).getOrElse("")
    countryWeighting.getOrElse(country, defaultWeight)
  }

  student match {
    case s: YoungStudent => retrieveWeight(s)
    case _ => defaultWeight
  }
}

def calculateStudentRank(student: Option[Student], note: Int): Option[Char] =
  student.flatMap(s => calculateRank(note, getWeighting(s)))

val student = Some { YoungStudent("Peter Griffin", Option(new IdCard(9, Option("FR")))) }
calculateStudentRank(student, 20) // Some(A)
calculateStudentRank(student, 4) // Some(D)
calculateStudentRank(student, 5) // Some(C)
calculateStudentRank(student, 10) // Some(B)
calculateStudentRank(student, 16) // Some(A)
calculateStudentRank(student, 14) // Some(A)
calculateStudentRank(student, 21) // Some(A)
calculateStudentRank(None, 20)
```

11) Don't forget to add/update unit tests to validate your work
* To do so extract your solution into a scala class
* Then write tests by using scalatest
```scala
class StudentRankCalculatorSpec extends AnyFlatSpec {
    "calculateRank" should "Return the correct rank for a given grade and person" in {
    
        val student = Some { YoungStudent("Peter Griffin", Option(new IdCard(9, Option("FR")))) }
    
        assert(calculateStudentRank(student, 4) === Some('D'))
        assert(calculateStudentRank(student, 5) === Some('C'))
        assert(calculateStudentRank(student, 10) === Some('B'))
        assert(calculateStudentRank(studcalculateStudentRankent, 16) === Some('A'))
    
        // test the case >=14 && <15 to prove our bugfix works
        assert(calculateStudentRank(student, 14) === Some('A'))
        // test the case >20 to prove our bugfix works
        assert(calculateStudentRank(student, 21) === Some('A'))
    }
}
```