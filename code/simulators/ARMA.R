#source("./test_methods.R")

arma_simulator = function(n_obs, param)
{
  data = arima.sim(n = n_obs, list(ar=param[[1]],ma=param[[2]]))
  data <- as.data.frame(data)
  colnames(data) = "y"
  return(data)
}

n_obs_list = c(1000,5000)

for (n_obs in n_obs_list) {
  
  this_arma_simulator = partial(arma_simulator,n_obs=n_obs,param=list(c(0.3),c(0.45)))
  this_loss = simulation_run(this_arma_simulator, n_iter = 2)
  
  this_pbloss  = this_loss$pbloss
  this_cdeloss = this_loss$cdeloss
  
  write_rds(this_pbloss, paste0("../results/PBLOSS_ARMA_11_",n_obs,"obs.rds"))
  write_rds(this_pbloss, paste0("../results/CDELOSS_ARMA_11_",n_obs,"obs.rds"))
  
}




