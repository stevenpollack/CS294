package hw2

import BIDMat.{Mat, FMat, DMat, IMat, CMat, BMat, CSMat, SMat, SDMat, GMat, GIMat, GSMat, HMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._
import BIDMat.Solvers._
import BIDMat.Plotting._

/* Assume the data comes in form review_i = (t^{i,1}, t^{i,2}, ..., t_{i,n}, r_i)^{T}
 * where t_i  are the token counts and r_i is the observed rating. This doesn't assume
 * that betaHat is an (n x 1) column vector 
 */

object LossFunctions {
	def absError(betaHat: DMat, reviews: DMat): DMat = {
		val inflatedBetaHat = DMat(1, betaHat.length+1, betaHat.data :+ -1.0) // append -1
		abs(inflatedBetaHat * reviews)
	}
	
	def squaredError(betaHat: DMat, reviews: DMat): DMat = {
		absError(betaHat,reviews).ddMatOpScalar(2.0,math.pow(_,_),null)
	}

	def gradSquaredError(betaHat: DMat, reviews: DMat): DMat = {
		val inflatedBetaHat = DMat(1, betaHat.length+1, betaHat.data :+ -1.0) // append -1
		val error = inflatedBetaHat * reviews // yields a row vector 
		val inflatedError = DMat(1, error.length+1, error.data :+ 0.0)

		2 * inflatedError * reviews
	}
}

class SGD(val numOfFeatures: Int, val stepSize: DMat, gradLossFxn: (DMat,DMat)=>DMat, babyTrainingSet: DMat) {
	if (stepSize.length > 1) { // allow for dynamic learning rate
		val maxSteps = alpha.length
	} else {
		val maxSteps = -1
	}

	override def toString(): String = {
		if (maxSteps > -1) {
			"Initialized SGD with dynamic learning rate, and termination clause: "+maxSteps+" steps"
		} else {
			"Initialized SGD with scalar learning rate: "+stepSize
		}
	}

	var betaHat = initializeBetaHat(babyTrainingSet)
	if (betaHat.length != numOfFeatures) { // in this case, we initialized on a small set
		// so fill out betaHat
		betaHat = DMat(numOfFeatures, 1, betaHat.data :+ dzeros(1,numOfFeatures-betaHat.length) )
	}
}

object SGD {
	def initializeBetaHat(reviews: DMat) = {
		// regardless of the loss, do least squares on a chunk of the data set
		// figure out a smart way to do this with less transpositions.
		val numOfFeatures = reviews.nrows
		val reviewsT = t(reviews)
		val X = reviewsT(?,0 to numOfFeatures -2)
		val Y = reviewsT(?,numOfFeatures-1)
		val betaHat = inv(t(X)*X) * t(X)*Y 
		betaHat	
	}
}

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