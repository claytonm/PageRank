package pageRank

import breeze.linalg._
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object HITS {

  def createLinkMatrix(nodes: Int, graph: Array[Tuple2[Int, Int]]): DenseMatrix[Double] = {
    val L = DenseMatrix.zeros[Double](nodes, nodes)
    for (row <- 0 to nodes - 1) {
      for (col <- 0 to nodes - 1) {
        if (graph.contains(Tuple2(row, col))) {
          L(row to row, col to col) := 1.0
        }
      }
    }
    L
  }

  def initializeHubbiness(nodes: Int): DenseVector[Double] = {
    DenseVector.fill(nodes){1.0}
  }

  def computeAuthority(L: DenseMatrix[Double], h: DenseVector[Double]): DenseVector[Double] = {
    L.t * h
  }

  def updateHubbiness(L: DenseMatrix[Double], a: DenseVector[Double]): DenseVector[Double] = {
    var h = L * a
    val maxH = max(h)
    h/maxH
  }

  def HITS(nodes: Int, graph: Array[Tuple2[Int, Int]],
           iterations: Int = 40, lambda: Int = 1, mu: Int = 1): DenseVector[Double] = {
    val L = createLinkMatrix(nodes, graph)
    var h = initializeHubbiness(nodes)
    for (i <- 1 to iterations) {
      var a = computeAuthority(L, h)
      h = updateHubbiness(L, a)
    }
    h
  }
}
