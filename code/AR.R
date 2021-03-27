source("load_previous_code.R")
############################################################

library(tidyverse)

ar_simulator = function(n_obs, param)
{
  data = arima.sim(n = n_obs, list(ar = param))
  data <- as.data.frame(data)
  colnames(data) = "y"
}

params = list(c(0.85), c(0.6))
n_obs = list(5000, 5000)
for(ii in length(params))
{
  this_ar_simulator = function() ar_simulator(n_obs[[ii]], params[[ii]])
  dfloss_ar = simulation_run(this_ar_simulator, n_iter = 100)
  arq_name = paste("./simulations/")
  write_rds(dfloss_ar, "./simulations/LOSS_AR_1_5000obs")
}



