rm(list=ls()) # clean R environment
cat("\f") # clear the console

# Exercise 3
source('./assignment3.R')

AUROC.vector = vector() # empty auroc scores list
max.scores.matrices = list() # empty list of max scores matrices from the mcmc
mcmc.max.scores.vector = vector() # empty max scores vector
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
  SCORES_vector = unlist(Sim[[2]]) # get the vector of the graph scores
  # Doubt should we extract the max out of the bge score or the AUC?
  SIM_max_score_idx = which.max(SCORES_vector) # get the idx where the max it
  SIM_max_score_matrix = Sim[[1]][[SIM_max_score_idx]] # get the highest scoring matrix
  max.scores.matrices[[idx]] = SIM_max_score_matrix # append to the list
  mcmc.max.scores.vector[idx] = SCORES_vector[SIM_max_score_idx] # save the max score vector
  # End doubt
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

# Printerino
# means computations
cat('The mean of the AUROC of m=20 is: ', mean(AUROC.vector[1:5]), '\n')
cat('The mean of the AUROC of m=50 is: ', mean(AUROC.vector[6:10]), '\n')
cat('The mean of the AUROC of m=100 is: ', mean(AUROC.vector[11:15]), '\n')
# std computations
cat('The std of the AUROC of m=20 is: ', sqrt(var(AUROC.vector[1:5])), '\n')
cat('The std of the AUROC of m=50 is: ', sqrt(var(AUROC.vector[6:10])), '\n')
cat('The std of the AUROC of m=100 is: ', sqrt(var(AUROC.vector[11:15])), '\n')
