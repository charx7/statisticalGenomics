# Exercise 3
source('./assignment3.R')

AUROC.vector = vector() # empty auroc scores list
# Run MCMC for the 15 datasets
for (idx in 1:15) {
  progress(idx)
  curr_data = data.list[[idx]]

  n_nodes = nrow(curr_data)  #  no. of network nodes
  # Generate an empty incidence matrix
  start_incidence = matrix(0, n_nodes,n_nodes)

  # Run the MCMC algo
  Sim = strMCMC(curr_data, start_incidence, iterations, step_save)
  n   = length(Sim[[1]]) # Get length of the chain
  SCORES_undirected = matrix(0,11,11) # declare an empty edge score matrix
  for (i in 1:n){
    # Sum the number of apperances on each score
    SCORES_undirected = (SCORES_undirected + Sim[[1]][[i]] + t(Sim[[1]][[i]]))
  }
  SCORES_undirected = SCORES_undirected/n # divivde by n to get the posterior edge score matrix

  true_Net = make_true_Net() # get the true network structure
  true_Net = true_Net + t(true_Net) # get also the simmetric true edges
  AUROC = compute_AUROC(SCORES_undirected,true_Net) # calculate the AUC
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
