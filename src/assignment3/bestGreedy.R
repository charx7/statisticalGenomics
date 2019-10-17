source('./assignment3.R') # source pre-processing script
# chdir = TRUE is needed because you are sourcing other files using the relative file loc inside the directory of the 
# sourcing file
source('../greedy_search/greedy_search.R', chdir = TRUE) # source the greedy search algo func

library(parallel)
no_cores <- detectCores()
greedy_list = mclapply(data.list, FUN = greedy_search, mc.preschedule = T, mc.cores = no_cores)
