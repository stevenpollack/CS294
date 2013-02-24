package hw2

import BIDMat.{Mat, FMat, DMat, IMat, CMat, BMat, CSMat, SMat, SDMat, GMat, GIMat, GSMat, HMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._
import BIDMat.Solvers._
import BIDMat.Plotting._
import hw2.LossFunctions._


class SGD(val numOfFeatures: Int, stepSize: Int => Float, gradLossFxn: (FMat,SMat,FMat) => FMat) {

	var betaHat = FMat(zeros(numOfFeatures,1)) // initialize betaHat to zeros
	var stepCount = 0

	val maxSteps = 1e4
	val convergenceThreshold = 1e-7

	var oneNormConvergence = false
	var twoNormConvergence = false
	

	def updateBetaHat(X: SMat, Y: FMat) = {
		val n = Y.length
		
		// calculate new betaHat
		val newBetaHat = this.betaHat - ( stepSize(stepCount+1) / n) *@ sum(gradLossFxn(this.betaHat, X, Y),2)
		println("betaHat is: "+this.betaHat+", grad is:"+sum(gradLossFxn(this.betaHat, X, Y),2))
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