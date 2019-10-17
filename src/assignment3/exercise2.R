rm(list=ls()) # clean R environment
cat("\f") # clear the console

source('./assignment3.R') # source pre-processing script

library('GeneNet')

# Exercise 2 
true_Net <- make_true_Net() # get the true net value
true_Net <- true_Net + t(true_Net) # get the simmetric true network

AUROC.vector.shrunk = vector() # empty auroc scores vector
AUROC.vector = vector() 
for (idx in 1:15) {
  curr_data = data.list[[idx]] # get current data matrix
  
  estimated.pcor = cor2pcor(cor(t(curr_data))) # compute the pcor without the shrinckage approach
  estimated.pcor.shrunk = pcor.shrink(t(curr_data)) # compute with the shrinkage approach
  matrix.estimated.pcor.shrunk = estimated.pcor.shrunk[1:11,1:11] # transform to matrix data type
  
  # Get the absolute values
  matrix.estimated.pcor.shrunk = abs(matrix.estimated.pcor.shrunk) # abs
  estimated.pcor = abs(estimated.pcor) # abs
  
  # auc for the shrunk matrix
  AUROC.shrunk = compute_AUROC(matrix.estimated.pcor.shrunk, true_Net) # compute the ROC
  AUROC.vector.shrunk = c(AUROC.vector.shrunk, AUROC.shrunk) # append the current AUROC
  # auc for the non-shrunk matrix
  AUROC = compute_AUROC(estimated.pcor, true_Net)
  AUROC.vector = c(AUROC.vector, AUROC) # append the current AUROC
}

# Printerino
# means computations
cat('Statistics for the shrunk pcors: \n')
cat('The mean of the AUROC of m=20 is: ', mean(AUROC.vector.shrunk[1:5]), '\n')
cat('The mean of the AUROC of m=50 is: ', mean(AUROC.vector.shrunk[6:10]), '\n')
cat('The mean of the AUROC of m=100 is: ', mean(AUROC.vector.shrunk[11:15]), '\n')
# std computations
cat('The std of the AUROC of m=20 is: ', sqrt(var(AUROC.vector.shrunk[1:5])), '\n')
cat('The std of the AUROC of m=50 is: ', sqrt(var(AUROC.vector.shrunk[6:10])), '\n')
cat('The std of the AUROC of m=100 is: ', sqrt(var(AUROC.vector.shrunk[11:15])), '\n')

# means computations
cat('Statistics for the non-shrunk pcors: \n')
cat('The mean of the AUROC of m=20 is: ', mean(AUROC.vector[1:5]), '\n')
cat('The mean of the AUROC of m=50 is: ', mean(AUROC.vector[6:10]), '\n')
cat('The mean of the AUROC of m=100 is: ', mean(AUROC.vector[11:15]), '\n')
# std computations
cat('The std of the AUROC of m=20 is: ', sqrt(var(AUROC.vector[1:5])), '\n')
cat('The std of the AUROC of m=50 is: ', sqrt(var(AUROC.vector[6:10])), '\n')
cat('The std of the AUROC of m=100 is: ', sqrt(var(AUROC.vector[11:15])), '\n')
