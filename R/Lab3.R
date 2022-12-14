#' euclidean of two Numbers
#'
#' @param a A number.
#' @param b A number.
#'
#' @return A number.
#' @export
#'
#' @examples
#' euclidean(100,1000)
#' euclidean(123612, 13892347912)
euclidean <- function(a,b){
  if(a<0)a <- a/-1
  if(b<0)a <- b/-1
  stopifnot(is.numeric(a),is.numeric(b))
  answer <-  1
 loopCounter <- 2
while( loopCounter <= a){
  modCal1= a%%loopCounter
  modCal2= b%%loopCounter
    if(modCal1==0 && modCal2==0){
          answer <- loopCounter
    }
    loopCounter=loopCounter+1
}
 return(answer)
}






#' this is the solution for dijkstra Algorithm
#'
#' @param wiki_graph A DataFrame
#' @param init_node A Number
#'
#' @return A Vector
#' @export
#'
#' @examples
#' wiki_graph <-data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#'  v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'  w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#' dijkstra(wiki_graph, 1)
#' dijkstra(wiki_graph, 3)

dijkstra <- function(wiki_graph, init_node){
  stopifnot(!is.null(wiki_graph$v1), !is.null(wiki_graph$v2), !is.null(wiki_graph$w), init_node %in% unique(wiki_graph[[1]]))
  stopifnot(is.data.frame(wiki_graph), is.numeric(init_node))
  uniqueVal <- unique(wiki_graph[[1]]) #nodes
  finalFrame = data.frame("node"= uniqueVal,"distance"= rep(Inf, length(uniqueVal)), "visited"=  rep(FALSE, length(uniqueVal)) )
  finalFrame[[2]][which(finalFrame[[1]]==init_node)]=0
  finalFrame
  visited <- c()
  while(length(uniqueVal)>0){
    minIndex <-as.numeric(min(finalFrame[[2]][finalFrame[[3]]==FALSE]))
    initialNode <- finalFrame$node[finalFrame$visited==FALSE & finalFrame$distance == minIndex][1]

    finalFrame[[3]][which(finalFrame[[1]]==initialNode)] <- TRUE
    nabours <- wiki_graph$v2[which(wiki_graph$v1==initialNode)]
    nabours <- nabours[nabours %in% uniqueVal]
    for(item in nabours){
      distanCalc <- wiki_graph$w[wiki_graph$v1==initialNode & wiki_graph$v2==item]
      distanCalc= distanCalc + finalFrame$distance[finalFrame$node==initialNode]
      oldDistance <- finalFrame$distance[finalFrame$node==item]
      oldDistance
      if(distanCalc < oldDistance){
        finalFrame$distance[which(finalFrame$node==item)]=distanCalc
        finalFrame
      }
    }
    uniqueVal <- uniqueVal[uniqueVal!= initialNode]
  }

  return(finalFrame$distance)
}





