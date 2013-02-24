import testobjects.Person
import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.prop._

class PersonSpec extends FlatSpec {
	intercept[IllegalArgumentException] {
		val testPerson = new Person(-1,1)	
	}

	val testPerson = new Person(1,1)
	intercept[IllegalArgumentException]{
		testPerson.nameIs("","Doe")
	}

	expect("John") {
		testPerson.nameIs("John","Doe")
		testPerson.first
	}
	
}

class SomeOtherTest extends FreeSpec with PropertyChecks with ShouldMatchers {
	"Simple Integers" - {
		"should have some uniqueness" in {
			(1) should be (1)
		}
	}
	// 	"should exhibit associative behavior" - {
	// 		"additive associativity:" in {
	// 			forAll { (a: Int, b: Int, c: Int) =>
	// 				((a+b)+c) should be (a+(b+c))
	// 			}	
	// 		}
	// 		"multiplicative associativity:" in {
	// 			forAll {(a: Int, b: Int, c: Int) =>
	// 				((a*b)*c) should be (a*(b*c))
	// 			}
	// 		}
	// 	}

	// 	"should exhibit commutativity" - {
	// 		"additive associativity:" in {
	// 			forAll { (a: Int, b: Int) =>
	// 				(a+b) should be (b+a)	
	// 			}
	// 		}
	// 		"multiplicative associativity:" in {
	// 			pending
	// 		}
	// 	}

	// 	"have that (-a^2)*(-b^2) > 0" - {
	// 		forAll{ (a: Int, b: Int) =>
	// 			whenever (a < 0 && b < 0) {
	// 				val product: Double = (a toDouble)*(b toDouble)
	// 				product should be >= (0.0)
	// 			}

	// 		}
	// 	}
	// }
}

class SomeTest extends FeatureSpec with ShouldMatchers with GivenWhenThen {
	feature ("ShouldMatcher provide nice error messages"){
	     scenario ("Making a correct assertion"){
		      given ("a number")
		      val number = 5
		      and ("an other number")
		      val otherNumber = 23
		      when ("multiplying one with the other")
		      val result = number * otherNumber
		      and ("the other way round")
		      val otherResult = otherNumber * number
		      then ("the results will be the same")
		      result should equal (otherResult)
	    }
	}
}
