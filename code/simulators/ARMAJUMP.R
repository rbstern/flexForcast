n_obs_list=c(1000,5000)
n_iter=100

armajump_simulator = function(n_obs, data, k){
  
  if (n_obs!=1000 & n_obs!=5000){
    stop('For this simulation, it is only possible to choose 1000 or 5000 observations.')
  }

  this_data = t(data[k,2:(n_obs+1)])
  this_data <- as.data.frame(this_data)
  colnames(this_data)="y"
  
  return(this_data)
  
}

for (n_obs in n_obs_list) {
  
  data <- read.csv(paste0('../data/arma_jump_simulations_',n_obs,'obs.csv'))
  
  this_armajump_simulator = partial(armajump_simulator,n_obs=n_obs,data=data)
  this_loss = simulation_run(this_armajump_simulator, n_iter = n_iter, read=TRUE)
  
  this_pbloss  = this_loss$pbloss
  this_cdeloss = this_loss$cdeloss
  
  write_rds(this_pbloss, paste0("../results/PBLOSS_ARMAJUMP_",n_obs,"obs.rds"))
  write_rds(this_pbloss, paste0("../results/CDELOSS_ARMAJUMP_",n_obs,"obs.rds"))
  
}
