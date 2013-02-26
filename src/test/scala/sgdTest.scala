import hw2.twoNormSGD

import BIDMat.{Mat, FMat, DMat, IMat, CMat, BMat, CSMat, SMat, SDMat, GMat, GIMat, GSMat, HMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._
import BIDMat.Solvers._
import BIDMat.Plotting._

import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.prop._

class L2SGDSuite extends FreeSpec with PropertyChecks with ShouldMatchers {
	"A Regression Model" - {
		"should predict shit right" in {
			val n = 1e3.toInt

			val X = normrnd(0,1,1,n)
			val eps = normrnd(0,5f,1,n)

			val data = sparse(X on X *@ X)
			val Y = (X + X*@X + eps).t

			val test0 = new twoNormSGD(0,2,0)
			val test1 = new twoNormSGD(1,2,0)
			val test2 = new twoNormSGD(2,2,0)

			while (test0.stepCount < 1e5) {
				for (j <- 0 until 10) {
					val seq = icol(j*100 until (j+1)*100)
					test0.runUpdateOnBatch(data(?,seq), Y(seq))
					test1.runUpdateOnBatch(data(?,seq), Y(seq))
					test2.runUpdateOnBatch(data(?,seq), Y(seq))
				}
			}
			//test.betaHat should be (col(0,0))
			abs(test0.betaHat(0))(0) should be (1f)
			abs(test0.betaHat(0)-test1.betaHat(0))(0) should be < 1e-5f
			abs(test1.betaHat(0))(0) should be (1f)
		}
	}
}