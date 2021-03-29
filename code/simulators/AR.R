#source("./test_methods.R")

n_iter=100
param=c(0.85)
n_obs_list = c(1000,5000)

ar_simulator = function(n_obs, param)
{
  data = arima.sim(n = n_obs, list(ar = param))
  data <- as.data.frame(data)
  colnames(data) = "y"
  return(data)
}

#ar_simulator1 = partial(ar_simulator,n_obs=1000,param=c(0.85))
#ar_simulator2 = partial(ar_simulator,n_obs=5000,param=c(0.85))

for (n_obs in n_obs_list) {
  
  this_ar_simulator = partial(ar_simulator,n_obs=n_obs,param=param)
  this_loss = simulation_run(this_ar_simulator, n_iter = n_iter)
  
  this_pbloss  = this_loss$pbloss
  this_cdeloss = this_loss$cdeloss
  
  write_rds(this_pbloss, paste0("../results/PBLOSS_AR_1_",n_obs,"obs.rds"))
  write_rds(this_pbloss, paste0("../results/CDELOSS_AR_1_",n_obs,"obs.rds"))
  
}




