import hw2.SGD
import hw2.LossFunctions._

import BIDMat.{Mat, FMat, DMat, IMat, CMat, BMat, CSMat, SMat, SDMat, GMat, GIMat, GSMat, HMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._
import BIDMat.Solvers._
import BIDMat.Plotting._

import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.prop._

class SGDSuite extends FreeSpec with PropertyChecks with ShouldMatchers {
	"A Regression Model" - {
		"should predict shit right" in {
			val n = 1e5

			val X = normrnd(0,1,1,n.toInt)
			val Z = normrnd(0,25,1,n.toInt)
			val data = sparse(X on X *@ X)
			val Y = (X + X*@X + Z).t

			val sgd = new SGD(2, x => 1/(x.toFloat), gradSquaredError)

			for (j <- 0 until data.ncols) {
				sgd.updateBetaHat(data(?,j),Y(j))
				println(sgd.betaHat)
			}

			(sum(sgd.betaHat - col(1,1))(0)) should be (0f)
			
		}
		"something something" - {
			1 should be (1)
		}

	}
}