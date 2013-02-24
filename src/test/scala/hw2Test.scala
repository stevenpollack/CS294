import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.prop._

class ModelSuite extends FreeSpec with PropertyChecks with ShouldMatchers {
	"A Regression Model" - {
		"should take a non-null loss function as input" - {
			pending
		}

		"should take a block of design matrix to initialize" - { //(documents in columns and features (words) in rows)
			pending 
		}

		"takes additional design matrix blocks and updates betas" - {
			pending
		}

		"predicts y.hat from new data passed to it" - {
			pending
		}

		"support both lasso and ridge penalties with varying lambda values" - {
			pending 
		}
	}
}	