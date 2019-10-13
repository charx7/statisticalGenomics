#install.packages("expm")
library(expm)

Ancestor <- function(Incidence){
  n <- dim(Incidence)[1]
  E <- Incidence
  for (i in 2:n){
    E <- E + Incidence%^%i
  }
  E <- t(E)
  A <- matrix(0, nrow = n, ncol = n)
  for (i in 1:n){
    for (j in 1:n){
      A[i,j] <- min(c(E[i,j],1))
    }
  }
  return(A)
}

DeletionList <- function(Incidence){
  Deletion <- NULL
  for (i in 1:dim(Incidence)[1]){
    for (j in 1:dim(Incidence)[2])
    if (Incidence[i,j] == 1){
      Deletion <- c(Deletion, paste("The edge from ", i, " to ", j, " can be deleted"))
    }
  }
  return(Deletion)
}

AdditionList <- function(Incidence){
  S <- (matrix(1, nrow = dim(Incidence)[1], ncol = dim(Incidence)[2]) - diag(dim(Incidence)[1]) - Incidence) - Ancestor(Incidence)
  Addition <- NULL
  for (i in 1:dim(Incidence)[1]){
    for (j in 1:dim(Incidence)[2])
      if (S[i,j] == 1 & i != j){
        Addition <- c(Addition, paste("The edge from ", i, " to ", j, " can be added"))
      }
  }
  return(Addition)
}

ReversalList <- function(Incidence){
  S <- Incidence - t(t(Incidence)%*%Ancestor(Incidence))
  Reversal <- NULL
  for (i in 1:dim(Incidence)[1]){
    for (j in 1:dim(Incidence)[2])
      if (S[i,j] == 1 && i != j){
        Reversal <- c(Reversal, paste("The edge from ", i, " to ", j, " can be reversed"))
      }
  }
  return(Reversal)
}

SingleEdgeOperationList <- function(Incidence){
  SingleEdgeOperation <- c(DeletionList(Incidence), AdditionList(Incidence), ReversalList(Incidence))
  return(SingleEdgeOperation)
}

NeighborgraphsList <- function(Incidence, data){
  NeighborList <- list("Incidence" = list(), "Ancestor" = list(), "BGe" = NULL)
  X <- SingleEdgeOperationList(Incidence)
      for (k in 1:length(X)){
        Incidence_new <- Incidence
        for (i in 1:dim(Incidence)[1]){
          for (j in 1:dim(Incidence)[2]){
        if (X[k] == paste("The edge from ", i, " to ", j, " can be deleted")){
          Incidence_new[i,j] <- 0
        }
        if (X[k] == paste("The edge from ", i, " to ", j, " can be added")){
          Incidence_new[i,j] <- 1
        }
        if (X[k] == paste("The edge from ", i, " to ", j, " can be reversed")){
          Incidence_new[i,j] <- 0
          Incidence_new[j,i] <- 1
        }
        NeighborList$Incidence[[k]] <- Incidence_new
        #NeighborList$Ancestor[[k]] <- Ancestor(Incidence_new)
        NeighborList$BGe[[k]] <- logBGe(data, Incidence_new)
      }
    }
  }
  return(NeighborList)
}

Incidence <- rbind(c(0,1,1,0,0,0,1), c(0,0,0,0,0,0,1), c(0,0,0,1,0,0,0), c(0,1,0,0,1,1,0), c(0,0,0,0,0,0,0), c(0,1,0,0,1,0,0), c(0,0,0,0,0,0,0))
