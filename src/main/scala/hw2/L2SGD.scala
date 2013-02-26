package hw2

import BIDMat.{Mat, FMat, DMat, IMat, CMat, BMat, CSMat, SMat, SDMat, GMat, GIMat, GSMat, HMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._
import BIDMat.Solvers._
import BIDMat.Plotting._

class twoNormSGD(val normID: Int = 0, numOfFeatures: Int, val lambda: Float = 0.1f) {
	import twoNormSGD._

	// normID = {0,1,2} = {L2, ridge, LASSO}
	var stepCount = 0
	var diff = 1d //twoNorm(betaHat - col(1,1))
	var hasNotConverged = true
	var betaHat = zeros(numOfFeatures,1)

	def runUpdateOnBatch(X: SMat, Y: FMat) = { // should be overloaded to handle no betaHat
		this.stepCount += 1	

		val update: FMat = this.normID match {
			case 0 => twoNormSGDUpdate(this.stepCount, this.betaHat, X, Y)
			case 1 => ridgeSGDUpdate(this.lambda, this.stepCount, this.betaHat, X, Y)
			case 2 => lassoSGDUpdate(this.lambda, this.stepCount, this.betaHat, X, Y)
		}

		this.diff = sum(abs(this.betaHat-update))(0)
		this.hasNotConverged = (this.diff > 1e-5)
		this.betaHat = update
		
		/*
		println("==============")
		println("step: " + this.stepCount)
		println("diff: " + this.diff)
		println("betaHat:" + this.betaHat)*/
	}
}

object twoNormSGD {

	def calculateResiduals(betaHat: FMat, X: SMat, Y: FMat) = {
		X.Tmult(betaHat, null) - Y
	}
	
	def twoNorm(x: FMat) = x dot x
	
	def meanL2Grad(betaHat: FMat, X: SMat, Y: FMat): FMat = {
		val resids = calculateResiduals(betaHat, X, Y)
		val n = X.ncols // # of reviews	
		X*resids /@ n
	}
	
	def twoNormSGDUpdate(step: Int, betaHat: FMat, X: SMat, Y: FMat) = {
		val learningRate = 0.1/step
		
		val update = betaHat - learningRate *@ meanL2Grad(betaHat, X, Y)
		assert (!update(0).isNaN & !update(1).isNaN, meanL2Grad(betaHat, X, Y))
		update
	}
	
	def ridgeSGDUpdate(lambda: Float, step: Int, betaHat: FMat, X: SMat, Y: FMat): FMat = {
		val betaLength = twoNorm(betaHat)
		val update = twoNormSGDUpdate(step,betaHat,X,Y) - 2 * lambda * betaHat
		update 
	}

	def lassoSGDUpdate(lambda: Float, step: Int, betaHat: FMat, X: SMat, Y: FMat): FMat = twoNormSGDUpdate(step,betaHat,X,Y) -  lambda * FMat(sign(betaHat))

}


