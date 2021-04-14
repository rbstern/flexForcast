#source("./test_methods.R")

n_iter=100
param=list(c(0.3,0.4),c(0.5,0.4))
n_obs_list = c(1000,5000)
lags=max(length(param[[1]]),length(param[[2]]))

arma_simulator = function(n_obs, param)
{
  data = arima.sim(n = n_obs, list(ar=param[[1]],ma=param[[2]]))
  data <- as.data.frame(data)
  colnames(data) = "y"
  return(data)
}


for (n_obs in n_obs_list) {
  
  this_simulator = partial(arma_simulator,n_obs=n_obs,param=param)
  this_loss = simulation_run(this_simulator, lags=lags, n_iter = n_iter)
  
  this_pbloss  = this_loss$pbloss
  this_cdeloss = this_loss$cdeloss
  
  write_rds(this_pbloss, paste0("../results/PBLOSS_ARMA_11_",n_obs,"obs.rds"))
  write_rds(this_pbloss, paste0("../results/CDELOSS_ARMA_11_",n_obs,"obs.rds"))
  
  processed_loss = process_loss_outputs(this_pbloss,this_cdeloss)
  write_rds(processed_loss,paste0("../results/processed/ARMA_11_",n_obs,"obs.rds"))
}




