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

class LossFunctions {
	/* holds vectorized versions of standard loss functions
	* all take column vectors (or matrices) as input and 
	* return column vectors
	*/

	def makeMbyNDiagMat(m: Int, n:Int, diagEntries: FMat): SMat = {
		assert(diagEntries.nrows == 1 || diagEntries.ncols == 1, "diagEntries is not a vector!")
		val minDim: Int = min(m,n)(0)
		assert(diagEntries.length == minDim, "diagEntries is not the right length!")
		sparse(0 until minDim, 0 until minDim, diagEntries, m, n)
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
		val m = X.nrows 
		val n = X.ncols
		val colScales = makeMbyNDiagMat(m,n,(2 * X.Tmult(betaHat,null) - Y))
		colScales * X  
	}
}


class SGD(val numOfFeatures: Int, stepSize: Int => Float, gradLossFxn: (FMat,SMat,FMat) => FMat) extends LossFunctions{

	var betaHat = FMat(zeros(numOfFeatures,1)) // initialize betaHat to zeros
	var stepCount = 0

	val maxSteps = 1e4
	val convergenceThreshold = 1e-7

	var oneNormConvergence = false
	var twoNormConvergence = false
	

	def updateBetaHat(X: SMat, Y: FMat) = {
		val n = Y.length
		
		// calculate new betaHat
		val newBetaHat = this.betaHat - ( stepSize(stepCount) / n) *@ sum(gradLossFxn(this.betaHat, X, Y),2)
		
		this.stepCount += 1	

		// check for convergence
		this.oneNormConvergence = (oneNorm(newBetaHat-this.betaHat)(0) < this.convergenceThreshold)
		this.twoNormConvergence = (twoNorm(newBetaHat-this.betaHat)(0) < this.convergenceThreshold)

		this.betaHat = newBetaHat
	}

}

/*object SGD with LossFunctions {

}
*/
class ADAGRAD(val stepSize: Float,lossFxn: (DMat,DMat)=>DMat, gradLossFxn: (DMat,DMat)=>DMat) {
	override def toString(): String = {
		"initializing ADAGRAD with stepSize = "+stepSize+" and loss function:"+lossFxn
	}
}

// object ADAGRAD {
// 	def update(oldBetaHat: DMat, stepSize: Float, tokens, rating, gradLossFxn,  oldGrad) {
// 		//x_{t+1} := oldBetaHat - stepSize diag(G_t)^{-1/2} g_t
// 		gt = gradLossFxn(betaHat, tokens, rating) 
		// val diagG = updateDiagG(oldDiagG,oldGrad)
// 		val invSqrtDiagG = diagG.ddMatOpScalar(-0.5,math.pow(_,_),null)

// 	}

// 	def updateDiagG(oldDiagG: FMat, oldGrad: FMat): FMat = {
// 		oldDiagG + oldGrad *@ oldGrad
// 	}
// }