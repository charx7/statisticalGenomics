rm(list=ls()) # clean R environment
cat("\f") # clear the console

source('./exercise3.R') # source the mcmc script
source('./bestGreedy.R') # source the best greedy script

#max.scores.matrices
#mcmc.max.scores.vector

# Get the true network
true_Net <- make_true_Net()
true_Net <- true_Net + t(true_Net)

bestAUCvec = vector()
for (idx in 1:15) {
  # select which method scored higher greedy or mcmc
  currMcmcScore = mcmc.max.scores.vector[idx]
  currGreedyScore = 3

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
cat('The mean of the AUROC of m=20 is: ', mean(greedyAUCvec[1:5]))
cat('The mean of the AUROC of m=50 is: ', mean(greedyAUCvec[6:10]))
cat('The mean of the AUROC of m=100 is: ', mean(greedyAUCvec[11:15]))
# std computations
cat('The std of the AUROC of m=20 is: ', sqrt(var(greedyAUCvec[1:5])))
cat('The std of the AUROC of m=50 is: ', sqrt(var(greedyAUCvec[6:10])))
cat('The std of the AUROC of m=100 is: ', sqrt(var(greedyAUCvec[11:15])))
