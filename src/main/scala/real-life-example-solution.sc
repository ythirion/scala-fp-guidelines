class IdCard(val number: Int, val country: Option[String])

abstract class Student(val name: String, val idCard: Option[IdCard])

case class YoungStudent(override val name: String, override val idCard: Option[IdCard]) extends Student(name, idCard)

case class OldStudent(override val name: String, override val idCard: Option[IdCard]) extends Student(name, idCard)

val countryWeighting = Map("FR" -> 1.1, "EN" -> 0.8)
val rankByNotes: Map[Range, Char] = Map(
  (0 until 5) -> 'D',
  (5 until 10) -> 'C',
  (10 until 14) -> 'B',
  (14 to 20) -> 'A'
)

val maxNote = 20
val defaultWeight = 1

def weightNote(note: Int, weighting: Double): Double = {
  val weightNote = note * weighting
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

val student = Some {
  YoungStudent("Peter Griffin", Option(new IdCard(9, Option("FR"))))
}
calculateStudentRank(student, 20)
calculateStudentRank(None, 20)
