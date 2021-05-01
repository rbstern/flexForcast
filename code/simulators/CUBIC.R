n_iter=100
n_obs_list = c(1000,5000)
lags=1

cubic.sim = function(n){
  
  #initial data point is distributed N(0,1)
  points = rnorm(1,0,1)
  
  # we will generate (n+order) points so as to remove the first (order) points
  for (i in 1:n){
    
    error_t = rnorm(1,0,0.5)
    lag = points[length(points)]
    
    if (lag > 1){
      new_point = 1
    } else if (lag < -1){
      new_point = -1
    } else {
      new_point = 0.5*lag^3+error_t
    }
    
    points = c(points,new_point)
  }
  
  points = points[2:length(points)]
  return(points)
}


cubic_simulator = function(n_obs)
{
  data = cubic.sim(n_obs)
  data <- as.data.frame(data)
  colnames(data) = "y"
  return(data)
}


for (n_obs in n_obs_list) {
  
  this_simulator = partial(cubic_simulator,
                           n_obs=n_obs)
  this_loss = simulation_run(this_simulator, lags=lags, n_iter = n_iter)
  
  this_pbloss  = this_loss$pbloss
  this_cdeloss = this_loss$cdeloss
  
  write_rds(this_pbloss, paste0("../results/PBLOSS_CUBIC_AR_",n_obs,"obs.rds"))
  write_rds(this_cdeloss, paste0("../results/CDELOSS_CUBIC_AR_",n_obs,"obs.rds"))
  
  processed_loss = process_loss_outputs(this_pbloss,this_cdeloss)
  write_rds(processed_loss,paste0("../results/processed/CUBIC_AR_",n_obs,"obs.rds"))
  
}
