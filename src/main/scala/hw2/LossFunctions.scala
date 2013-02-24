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
	def absError(betaHat: FMat, X: SMat, Y: FMat): FMat = abs(Y - X.Tmult(betaHat,null))

	def gradAbsError(betaHat: FMat, X: SMat, Y: FMat): FMat = {
		X * sign( X.Tmult(betaHat,null) - Y ) 
	}
	
	def squaredError(betaHat: FMat, X: SMat, Y: FMat): FMat = {
		// avoid casting issues math.pow() by manually squaring
		val absoluteE = absError(betaHat,X,Y) 
		absoluteE *@ absoluteE
	}

	def gradSquaredError(betaHat: FMat, X: SMat, Y: FMat): FMat = {
		// not sure how fast this matrix algebra will be. =(
		X * (2 * betaHat * X - Y) 
	}
}

object SGD {
	/* These don't matter any more -- initialize beta to 0
	def initializeBetaHat1(reviews: DMat) = {
		// regardless of the loss, do least squares on a chunk of the data set
		/* this version supposes Y = X beta,
		*  Y is a column vector
		*/
		val numOfFeatures = reviews.nrows
		val numOfReviews = reviews.ncols
		val reviewsT = reviews.t
		val X = reviewsT(?,0 until numOfFeatures -1)
		val Y = reviewsT(?,numOfFeatures-1)
		val betaHat = (X.t * X).inv * X.t * Y 
		betaHat
		

	}

	def initializeBetaHat2(reviews: DMat) = {
		/* this version suppose Y = beta X, 
		* Y is a row vector
		*/
		val numOfFeatures = reviews.nrows
		val numOfReviews = reviews.ncols
		val Yt = dzeros(numOfReviews,1)
		for (j <- 0 until numOfReviews) { // not sure if speed benefit for Yt.data vs. Yt(i)
			Yt.data(j) = reviews.data( (j+1)*numOfFeatures - 1 )
		}
		val X = reviews(0 until numOfFeatures-1,?)

		val betaHatTrans = (X * X.t).inv * X * Yt 
		betaHatTrans.t // return a column vector 
	} */
}

class SGD(val numOfFeatures: Int, val stepSize: DMat, gradLossFxn: (DMat,DMat)=>DMat, babyTrainingSet: DMat) {
	// if (stepSize.length > 1) { // allow for dynamic learning rate
	// 	val maxSteps = alpha.length
	// } else {
	// 	val maxSteps = -1
	// }

	// override def toString(): String = {
	// 	if (maxSteps > -1) {
	// 		"Initialized SGD with dynamic learning rate, and termination clause: "+maxSteps+" steps"
	// 	} else {
	// 		"Initialized SGD with scalar learning rate: "+stepSize
	// 	}
	// }

	// var betaHat = initializeBetaHat2(babyTrainingSet)
	// if (betaHat.length != numOfFeatures) { // in this case, we initialized on a small set
	// 	// so fill out betaHat
	// 	betaHat = betaHat.vertcat(dzeros(1,numOfFeatures-betaHat.length))
	// }
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