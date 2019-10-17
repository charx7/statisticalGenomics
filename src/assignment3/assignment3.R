source('./utils.R')
source('./MCMC.R')

#  Data Params
m_obs     = c(20, 50, 100) # numb of observations
var_noise = 1 # noise of the data
# MCMC params
iterations  = 20000 # Total no. of MCMC iterations per run
step_save   = 100 # Thin-out by a factor of 100

data.list = list()
data.idx = 0
for (idx in 1:3) {
  for (jdx in 1:5) {
    data.idx = data.idx + 1
    cur.obs = m_obs[idx] # get the current numb of observations
    cur.data = make_test_Data(cur.obs,var_noise) # generate data
    data.list[[data.idx]] = cur.data
  }
}
