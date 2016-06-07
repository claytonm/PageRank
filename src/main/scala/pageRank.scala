package pageRank

import breeze.linalg._
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object pageRank {

  def countNodes(filePath: String): Int = {
    var maxNode = 0
    for (line <- Source.fromFile(filePath).getLines()) {
      var currentUsers = line.split("\t")
      var maxCurrent = math.max(currentUsers(0).toInt, currentUsers(1).toInt)
      maxNode = math.max(maxNode, maxCurrent)
    }
    maxNode
  }

  def initializeR(nodes: Int): DenseVector[Double] = {
    DenseVector.fill(nodes){1/nodes.toDouble}
  }

  def readPairs(filePath: String): Array[(Int, Int)] = {
    var pairs = ArrayBuffer[(Int, Int)]()
    for (line <- Source.fromFile(filePath).getLines()) {
      var currentPair = line.split("\t")
      pairs = pairs += Tuple2(currentPair(0).toInt - 1, currentPair(1).toInt - 1)
    }
    pairs.toArray.distinct
  }

  def getDegrees(graph: Array[(Int, Int)]) = {
    graph.map{case(row, col) => row}.groupBy(identity).mapValues(_.size)
  }

  def getM(nodes: Int, degrees: Map[Int, Int], graph: Array[Tuple2[Int, Int]]): DenseMatrix[Double] = {
    val M = DenseMatrix.zeros[Double](nodes, nodes)
    val froms = degrees.keys
    for (row <- 0 to nodes - 1) {
      for (from <- froms) {
        if (graph.contains(Tuple2(from, row))) {
          M(row to row, from to from) := 1/degrees(from).toDouble
        }
      }
    }
    M
  }

  def updateR(beta: Double, nodes: Int, r: DenseVector[Double], M: DenseMatrix[Double]): DenseVector[Double] = {
    val ones = DenseVector.fill(nodes){(1 - beta)/nodes}
    ones + beta * M * r
  }

  def pageRank(beta: Double,
               nodes: Int,
               R: DenseVector[Double],
               M: DenseMatrix[Double],
               iterations: Int): DenseVector[Double] = {
    var r = R
    for (i <- 1 to iterations) {
      r = updateR(beta, nodes, r, M)
    }
    r
  }

  def topPages(n: Int, pageRank: DenseVector[Double]): Array[(Int, Double)] = {
    pageRank.
      toArray.
      zipWithIndex.
      sortBy{case(pageRank, index) => -1*pageRank}.
      map{case(pageRank, index) => (index, pageRank)}.
      take(n)
  }

  def bottomPages(n: Int, pageRank: DenseVector[Double]): Array[(Int, Double)] = {
    pageRank.
      toArray.
      zipWithIndex.
      sortBy{case(pageRank, index) => pageRank}.
      map{case(pageRank, index) => (index, pageRank)}.
      take(n)
  }


}

