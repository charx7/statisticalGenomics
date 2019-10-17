source("./Utils_Q2.R")
source('./Utils_Q1_3.R')
require(svMisc)

greedy_search = function(data) {
  # >> Init
  # Empty incidence matrix
  #Inc = matrix(0,6,6) # empty incidence matrix with 6 nodes
  Inc = matrix(0, dim(data)[1], dim(data)[1])
  
  # >> Params
  gList = list() # init empty vector
  numIter = 100
  bestGraph = Inc # append the first 'arbitrary graph' to the results vector
  bestScore = 0
  maxIter = 2
  
  # >> Main training loop
  for(idx in 1:maxIter) {
    currGraph = bestGraph # get current graph
    currScore = logBGe(data, currGraph) # compute the current score of the graph
    neigs = NeighborgraphsList(currGraph, data) # compute its neighbours
    scoreVector = unlist(neigs[3]) # retreive the bge score list
    max_idx = which(scoreVector == max(scoreVector)) # retreive the idx of the max bge
    max_idx = unlist(max_idx) # cast to scalar
    
    if (length(max_idx) > 1) {
      max_idx = max_idx[1] # if a tie then choose the first one "should be random
    }
    
    localMaxGraph = neigs[1]$Incidence[max_idx] # get the local max graph
    localMaxGraphScore = neigs[3]$BGe[max_idx] # get the local max score
    
    if (localMaxGraphScore > currScore) {
      bestGraph = localMaxGraph[[1]]
      bestScore = localMaxGraphScore
    } else {
      bestGraph = currGraph
      bestScore = currScore
    }
    progress(idx*100/maxIter)
  }
  return(list(bestGraph,bestScore))
}
# Example data
# # Generate Data
# n=6
# m = 30
# data = matrix(rnorm(n*m),n,m)
