source('./assignment3.R') # source pre-processing script

# Exercise 1 
true_Net <- make_true_Net() # get the true net value
true_Net <- true_Net + t(true_Net) # get the simmetric true network

# Compute the AUCROC for each data set on the data set list
AUROC.vector = vector() # empty auroc scores list
for (idx in 1:15) {
  curr_data = data.list[[idx]]
  corr_coef_matrix = cor(t(curr_data), method = 'pearson') # calculate the corr coefficient matrix
  C = abs(corr_coef_matrix) # get the abs value of each element

  AUROC = compute_AUROC(abs(C),true_Net) # compute the ROC
  AUROC.vector = c(AUROC.vector, AUROC) # append the current AUROC
}

# means computations
cat('The mean of the AUROC of m=20 is: ', mean(AUROC.vector[1:5]))
cat('The mean of the AUROC of m=50 is: ', mean(AUROC.vector[6:10]))
cat('The mean of the AUROC of m=100 is: ', mean(AUROC.vector[11:15]))
# std computations
cat('The std of the AUROC of m=20 is: ', sqrt(var(AUROC.vector[1:5])))
cat('The std of the AUROC of m=50 is: ', sqrt(var(AUROC.vector[6:10])))
cat('The std of the AUROC of m=100 is: ', sqrt(var(AUROC.vector[11:15])))
