rm(list=ls()) # clean R environment
cat("\f") # clear the console

source('./exercise3.R') # source the mcmc script
source('./bestGreedy.R') # source the best greedy script

# Get the true network
true_Net <- make_true_Net()
true_Net <- true_Net + t(true_Net)

results = list(list(), list(), list()) # empty lists vector
bestAUCvec = vector()
for (idx in 1:15) {
  # select which method scored higher greedy or mcmc
  currMcmcScore = mcmc.max.scores.vector[idx]
  currMcmcMatrix = max.scores.matrices[[idx]]
  currGreedyScore = greedy_list[[idx]][[2]]
  currGreedyMatrix = greedy_list[[idx]][[1]]
  
  if(currMcmcScore > currGreedyScore) {
    cat('MCMC had a better score \n')
    results[[1]][[idx]] = 'MCMC'
    results[[2]][[idx]] = currMcmcScore
    results[[3]][[idx]] = currMcmcMatrix
  } else {
    cat('Greedy search had a better score \n')
    results[[1]][[idx]] = 'Greedy'
    results[[2]][[idx]] = currGreedyScore
    results[[3]][[idx]] = currGreedyMatrix
  }
  
  # Calculate the AUC for the selected matrix
  currGraph = results[[3]][[idx]]
  currGraph = currGraph + t(currGraph)
  currAUC = compute_AUROC(currGraph, true_Net)
  bestAUCvec[idx] = currAUC
}

# Compute the AUCS
greedyAUCvec = vector()
for (idx in 1:15) {
  currGraph = greedy_list[[idx]][[1]]
  currGraph = currGraph+t(currGraph)
  currAUC = compute_AUROC(currGraph, true_Net)
  # compute the AUC
  greedyAUCvec = c(greedyAUCvec, currAUC)
}

# means computations
cat('The mean of the AUROC of m=20 is: ', mean(greedyAUCvec[1:5]), '\n')
cat('The mean of the AUROC of m=50 is: ', mean(greedyAUCvec[6:10]), '\n')
cat('The mean of the AUROC of m=100 is: ', mean(greedyAUCvec[11:15]), '\n')
# std computations
cat('The std of the AUROC of m=20 is: ', sqrt(var(greedyAUCvec[1:5])), '\n')
cat('The std of the AUROC of m=50 is: ', sqrt(var(greedyAUCvec[6:10])), '\n')
cat('The std of the AUROC of m=100 is: ', sqrt(var(greedyAUCvec[11:15])), '\n')

# means computations
cat('The mean of the AUROC of m=20 is: ', mean(bestAUCvec[1:5]), '\n')
cat('The mean of the AUROC of m=50 is: ', mean(bestAUCvec[6:10]), '\n')
cat('The mean of the AUROC of m=100 is: ', mean(bestAUCvec[11:15]), '\n')
# std computations
cat('The std of the AUROC of m=20 is: ', sqrt(var(bestAUCvec[1:5])), '\n')
cat('The std of the AUROC of m=50 is: ', sqrt(var(bestAUCvec[6:10])), '\n')
cat('The std of the AUROC of m=100 is: ', sqrt(var(bestAUCvec[11:15])), '\n')
