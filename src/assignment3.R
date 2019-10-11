rm(list=ls()) # clean R environment
cat("\f") # clear the console

source('./utils.R')
source('./MCMC.R')

#  Data Params
m_obs     <- 20 # numb of observations
var_noise <- 1 # noise of the data
data      <- make_test_Data(m_obs,var_noise) # generate data

n_nodes   <- nrow(data)  #  no. of network nodes
# MCMC params
iterations  <- 20000 # Total no. of MCMC iterations per run
step_save   <- 100 # Thin-out by a factor of 100

# Generate an empty incidence matrix
start_incidence <- matrix(0,n_nodes,n_nodes)

# Exercise 3
# Run the MCMC algo
Sim <- strMCMC(data, start_incidence, iterations, step_save)
n   <- length(Sim[[1]]) # Get length of the chain
SCORES_undirected <- matrix(0,11,11) # declare an empty edge score matrix
for (i in 1:n){
  # Sum the number of apperances on each score
  SCORES_undirected <- (SCORES_undirected + Sim[[1]][[i]] + t(Sim[[1]][[i]]))
}
SCORES_undirected <- SCORES_undirected/n # divivde by n to get the posterior edge score matrix

true_Net <- make_true_Net() # get the true network structure
true_Net <- true_Net + t(true_Net) # get also the simmetric true edges
AUROC <- compute_AUROC(SCORES_undirected,true_Net) # calculate the AUC

