n_obs_list=c(2500,10000)
n_iter=100
lags_garch=1

jumpdiffusion_simulator = function(n_obs, data, k){
  
  if (n_obs!=1000 & n_obs!=2500 & n_obs!=5000 & n_obs!=10000){
    stop('For this simulation, it is only possible to choose 1000, 2500, 5000 or 10000 observations.')
  }
  
  this_data=data[(n_obs*(k-1)+1):(n_obs*k),2:5]
  colnames(this_data)[4]="y"
  
  return(this_data)
  
}

for (n_obs in n_obs_list) {
  
  data <- read.csv(paste0('../data/jump_diffusion_simulations_',n_obs,'obs.csv'))
  
  this_simulator = partial(jumpdiffusion_simulator,n_obs=n_obs,data=data)
  this_loss = simulation_run(this_simulator, lags=lags_garch, n_iter = n_iter, read=TRUE)
  
  this_pbloss  = this_loss$pbloss
  this_cdeloss = this_loss$cdeloss
  
  write_rds(this_pbloss, paste0("../results/PBLOSS_JUMPDIFFUSION_",n_obs,"obs.rds"))
  write_rds(this_pbloss, paste0("../results/CDELOSS_JUMPDIFFUSION_",n_obs,"obs.rds"))
  
  processed_loss = process_loss_outputs(this_pbloss,this_cdeloss)
  write_rds(processed_loss,paste0("../results/processed/JUMPDIFFUSION_",n_obs,"obs.rds"))
}
