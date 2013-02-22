package testobjects

import scala.io.Source

class Person(var height: Double, var weight: Double) {
	require(this.height >= 0)
	require(this.weight >= 0)

	var first: String = ""
	var last: String = ""

	override def toString(): String = {
		"person's height is "+this.height+" and their weight is "+this.weight
	}

	def nameIs(first: String, last: String) = {
		require(!first.isEmpty)
		require(!last.isEmpty)
		this.first = first
		this.last = last
	}

}

object Person {
	def main(args: Array[String]): Unit = {
		val steven = new Person(6,150)
		println(steven.height+" "+steven.weight)
		steven.height = 6.1
		println(steven.height)
	}
}

class FileParser(val fileName: String) {
	require(!this.fileName.isEmpty)
}