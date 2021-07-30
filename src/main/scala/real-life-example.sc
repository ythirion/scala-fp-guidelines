class IdCard(val number: Int,val country: Option[String])

abstract class Person(val name: String, val idCard: Option[IdCard])
case class YoungStudent(override val name: String, override val idCard: Option[IdCard]) extends Person(name, idCard)
case class OldStudent(override val name: String, override val idCard: Option[IdCard]) extends Person(name, idCard)

// Boost Students notes for some countries ;-)
val countryWeighting = Map("FR" -> 1.1, "EN" -> 0.8)

def calculateStudentRank(person: Option[Person], note: Int): Option[Char] = {
  if(person.isEmpty)
    throw new IllegalArgumentException("Person is not defined")

  val country = if (person.isDefined && person.get.idCard.isDefined) person.get.idCard.get.country else ""

  if(country != ""){
    person match {
      case Some(p) =>
        p match {
          case OldStudent(name, idCard) =>
            note match {
              case it if it >= 0 && it < 5 => Some('D')
              case it if it >= 5 && it < 10 => Some('C')
              case it if it >= 10 && it < 14 => Some('B')
              case it if it >= 15 && it < 20 => Some('A')
              case _ => throw new IllegalArgumentException("Note not in range")
            }
          case YoungStudent(name, idCard) =>
            val weighting = countryWeighting(person.get.idCard.get.country.get)
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

val person = Some { YoungStudent("Peter Griffin", Option(new IdCard(9, Option("FR")))) }
calculateStudentRank(person, 10)