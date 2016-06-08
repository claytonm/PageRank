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

    val rank = pageRank.pageRank(beta, nodes, R, M, iterations)
    val top = pageRank.topPages(5, rank)
    val bottom = pageRank.bottomPages(5, rank)

    println("Top 5 pages (Page Rank Implementation):")
    top.foreach(println)
    println("Bottom 5 pages (Page Rank Implementation):")
    bottom.foreach(println)

    val h = HITS.HITS(nodes, graph)

    val topPages = pageRank.topPages(5, h)
    val bottomPages = pageRank.bottomPages(5, h)

    println("Top 5 pages (HITS Implementation):")
    topPages.foreach(println)
    println("Bottom 5 pages (HITS Implementation):")
    bottomPages.foreach(println)
  }
}





