#' Export an Adjacency Matrix to a graphml file
#'
#' @description
#' Saves a weighted adjacency matrix to a graphML using a specified minimum threshold.
#' The resulting graphml file can be visualized with programs such as Cytoscape
#'
#' @param adjMat A weighted adjacency matrix
#' @param filename Filename for the saved network.
#' @param nodeAttributes Data frame containing additional node information.
#' @param threshold Minimum threshold needed to create an edge.
#'
#' @references
#' https://github.com/iscb-dc-rsg/2016-summer-workshop/blob/master/3B-Hughitt-RNASeq-Coex-Network-Analysis/tutorial/README.Rmd
#'
#' @return An igraph object of the input adjecency network.
#' @export

adj2graphml <- function(adjMat,
                        filename = "network.graphml",
                        nodeAttributes,
                        threshold = 0.5) {

  adjMat <- adjMat[abs(adjMat) < threshold] <- 0

  g <- igraph::graph.adjacency(adjMat, mode = "undirected", weighted = T, diag = F)

  if (!is.null(nodeAttributes)) {
    for (colname in colnames(nodeAttributes)) {
      g <- igraph::set.vertex.attribute(g, colname, value=nodeAttributes[,colname])
    }
  }

  write.graph(g, filename, format = "graphml")
  return(g)
}
