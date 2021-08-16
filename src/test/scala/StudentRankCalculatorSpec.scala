import com.reallifeexample._
import org.scalatest.flatspec.AnyFlatSpec

class StudentRankCalculatorSpec extends AnyFlatSpec {

  "calculateRank" should "Return the correct rank for a given grade and person" in {

    val student = Some { YoungStudent("Peter Griffin", Option(new IdCard(9, Option("FR")))) }

    assert(StudentRankCalculator.calculateRank(student, 4) === Some('D'))
    assert(StudentRankCalculator.calculateRank(student, 5) === Some('C'))
    assert(StudentRankCalculator.calculateRank(student, 10) === Some('B'))
    assert(StudentRankCalculator.calculateRank(student, 16) === Some('A'))

    // test the case >=14 && <15 to prove our bugfix works
    assert(StudentRankCalculator.calculateRank(student, 14) === Some('A'))
    // test the case >20 to prove our bugfix works
    assert(StudentRankCalculator.calculateRank(student, 21) === Some('A'))

  }

}