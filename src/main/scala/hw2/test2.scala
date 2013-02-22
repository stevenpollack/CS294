class person(height: Double, weight: Double) {
	override def toString(): String = {
		"person's height is "+this.height+" and their weight is "+this.weight
	}
}

object person {
	def main(args: Array[String]): Unit = {
		val steven = new person(6,150)
	}
}