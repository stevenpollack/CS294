// package hw2.LossFunctions {
// 	import BIDMat.{Mat, FMat, DMat, IMat, CMat, BMat, CSMat, SMat, SDMat, GMat, GIMat, GSMat, HMat}
// 	import BIDMat.MatFunctions._
// 	import BIDMat.SciFunctions._
// 	import BIDMat.Solvers._
// 	import BIDMat.Plotting._

// 	object LossFunctions {
// 		def absError(betaHat: FMat, tokens: FMat, rating: Float): Float = {
// 			sum(abs(rating- betaHat *@ tokens))
// 		}
		
// 		def squaredError(betaHat: FMat, tokens: FMat, rating: Float): Float = {
// 			sum((rating- betaHat *@ tokens) *@ (rating- betaHat *@ tokens))
// 		}

// 		def gradSquaredError(betaHat: FMat, tokens: FMat, rating: Float): FMat = {
// 			2 * tokens *@ (sum(betaHat*@tokens) - rating)
// 		}
// 	}


// }

// package hw2

// import BIDMat.{Mat, FMat, DMat, IMat, CMat, BMat, CSMat, SMat, SDMat, GMat, GIMat, GSMat, HMat}
// import BIDMat.MatFunctions._
// import BIDMat.SciFunctions._
// import BIDMat.Solvers._
// import BIDMat.Plotting._



// abstract class RegressionModel(fxn: Int) {
// 	var betaHat: FMat
// 	var stepSize: Float	
// }


// object RegressionModel extends App {


// 	def batchGDUpdate(betaHat: FMat, tokensMat: FMat, ratings: FMat, stepSize: Float, gradLossFxn: (FMat,FMat,Float) => FMat): FMat = {
// 		val empAvg: Float = 0
// 		for (col <- (0,scores.length)) {
// 			empAvg += - stepSize*gradLossFxn(betaHat,tokens=tokensMat(,col),rating=ratings(col))
// 		}
// 		betaHat += math.pow(scores.length,-1)*empAvg
// 	}
// 	// def update(X: FMat) 

// 	// def initialize(X: FMat) 

// 	// def predict(X: FMat) 
// }