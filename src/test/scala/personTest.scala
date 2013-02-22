import testobjects.Person
import org.scalatest._

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