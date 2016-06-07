package pageRank

object Main {

  def main(args: Array[String]): Unit = {
    val filePath = "/Users/clay/education/mmds/data/graph.txt"
    val nodes = pageRank.countNodes(filePath)
    var R = pageRank.initializeR(nodes)
    val graph = pageRank.readPairs(filePath)
    val degrees = pageRank.getDegrees(graph)
    val M = pageRank.getM(nodes, degrees, graph)
    val beta = 0.8
    val iterations = 40

    R.toArray.zipWithIndex.foreach(println)

    val rank = pageRank.pageRank(beta, nodes, R, M, iterations)
    val top = pageRank.topPages(5, rank)
    val bottom = pageRank.bottomPages(5, rank)

    println("Top 5 pages:")
    top.foreach(println)
    println("Bottom 5 pages:")
    bottom.foreach(println)

  }

}





