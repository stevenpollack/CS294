package hw2

import BIDMat.{Mat, FMat, DMat, IMat, CMat, BMat, CSMat, SMat, SDMat, GMat, GIMat, GSMat, HMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._
import BIDMat.Solvers._
import BIDMat.Plotting._


abstract class RegressionModel(fxn: Int) {
	var betaHat: FMat
	var stepSize: Float	
}


object RegressionModel extends App {


	def batchGDUpdate(betaHat: FMat, reviews: DMat, stepSize: Float, gradLossFxn: (FMat,FMat,Float) => FMat): FMat = {
		val numOfFeatures = reviews.nrows-1
		val numOfReviews = reviews.ncols

		val empAvg: Float = 0
		
		// for (col <- (0,numOfReviews)) {
		// 	empAvg += - stepSize*gradLossFxn(betaHat,tokens=tokensMat(?,col),rating=ratings(col))
		// }
		betaHat + math.pow(numOfReviews,-1)*empAvg
	}
}	

