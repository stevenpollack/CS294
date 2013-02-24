package hw2

import BIDMat.{Mat, FMat, DMat, IMat, CMat, BMat, CSMat, SMat, SDMat, GMat, GIMat, GSMat, HMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._
import BIDMat.Solvers._
import BIDMat.Plotting._

/* Assume the data comes in form X = (c_1, c_2, ... c_m) where c_i = (n x 1)
 * column vectors contains token counts, and Y = (m x 1) column vector of ratings
 * associated with datum columns of X. 
 * NOTE: x.t * A = A.t * x = A Tmult(x,null), so let betaHat be a column vector
 * to take advantage of Tmult without transposing all the time
 */

object LossFunctions {
	/* holds vectorized versions of standard loss functions
	* all take column vectors (or matrices) as input and 
	* return column vectors
	*/

	def makeDiagMat(diagEntries: FMat): SMat = {
		assert(diagEntries.nrows == 1 || diagEntries.ncols == 1, "diagEntries is not a vector!")
		val n = diagEntries.length
		sparse(0 until n, 0 until n, diagEntries, n, n)
	}

	def oneNorm(x: FMat) = sum(abs(x))
	def twoNorm(x: FMat) = x dot x

	def absError(betaHat: FMat, X: SMat, Y: FMat): FMat = abs(Y - X.Tmult(betaHat,null))

	def gradAbsError(betaHat: FMat, X: SMat, Y: FMat): FMat = {
		X * sign( X.Tmult(betaHat,null) - Y ) 
	}
	
	def squaredError(betaHat: FMat, X: SMat, Y: FMat): FMat = {
		// avoid casting issues of math.pow() by manually squaring
		val absoluteE = absError(betaHat,X,Y) 
		absoluteE *@ absoluteE
	}

	def gradSquaredError(betaHat: FMat, X: SMat, Y: FMat): FMat = {
		// not sure how fast this matrix algebra will be. =(
		val numOfReviews = X.ncols
		val numOfFeatures = X.nrows
		val out = zeros(numOfFeatures,numOfReviews)

		val scaleFactor = 2 * (X.Tmult(betaHat,null) - Y)

		for (j <- 0 until numOfReviews) { // look into out.data if slow
			out(?,j) = scaleFactor(j)*full(X(?,j))
		}
		out
	}
}

