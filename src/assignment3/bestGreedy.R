source('./assignment3.R') # source pre-processing script
# chdir = TRUE is needed because you are sourcing other files using the relative file loc inside the directory of the 
# sourcing file
source('../greedy_search/greedy_search.R', chdir = TRUE) # source the greedy search algo func

library(parallel)
no_cores <- detectCores()
greedy_list = mclapply(data.list, FUN = greedy_search, mc.preschedule = T, mc.cores = no_cores)

# greedy_list = list()
# for (idx in 1:2) {
#   curr_data = data.list[[idx]]
#   best_greedy_graph = greedy_search(curr_data)
#   greedy_list[[idx]] = best_greedy_graph
#   cat('Done with n=', idx, '\n')
#}

# Get the true network
true_Net <- make_true_Net()
true_Net <- true_Net + t(true_Net)

# First dataset
m20List = greedy_list[c(1:5)]
m20AUCvec = vector()
for (idx in 1:5) {
  m20AUCvec = c(m20AUCvec, m20List[[idx]][[2]])
}
maxIdx = which.max(m20AUCvec)
maxScorem20Graph = m20List[[maxIdx]][[1]]
maxScorem20Graph_undirected = maxScorem20Graph+t(maxScorem20Graph)
AUROCm20 <- compute_AUROC(maxScorem20Graph_undirected, true_Net)

# Second dataset
m50List = greedy_list[c(6:10)]
m50AUCvec = vector()
for (idx in 1:5) {
  m50AUCvec = c(m50AUCvec, m50List[[idx]][[2]])
}
maxIdx = which.max(m50AUCvec)
maxScorem50Graph = m50List[[maxIdx]][[1]]
maxScorem50Graph_undirected = maxScorem50Graph+t(maxScorem50Graph)
AUROCm50 <- compute_AUROC(maxScorem50Graph_undirected, true_Net)

# Third dataset
m100List = greedy_list[c(11:15)]
m100AUCvec = vector()
for (idx in 1:5) {
  m100AUCvec = c(m100AUCvec, m100List[[idx]][[2]])
}
maxIdx = which.max(m100AUCvec)
maxScorem100Graph = m100List[[maxIdx]][[1]]
maxScorem100Graph_undirected = maxScorem100Graph+t(maxScorem100Graph)
AUROCm100 <- compute_AUROC(maxScorem100Graph_undirected, true_Net)

