package hw2

import BIDMat.{Mat, FMat, DMat, IMat, CMat, BMat, CSMat, SMat, SDMat, GMat, GIMat, GSMat, HMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._
import BIDMat.Solvers._
import BIDMat.Plotting._



class RegressionModel(fxn: Int) {
	def betaHat: col	
}

object RegressionModel extends App {
	def absErrorLossFxn(x: icol, y: icol) = sum(abs(x-y))
	
	def squaredErrorLossFxn(x: icol, y:icol) = sum((x-y) *@ (x-y))

	// def update(X: FMat) 

	// def initialize(X: FMat) 

	// def predict(X: FMat) 
}