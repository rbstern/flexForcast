c=0.1
ar=0.9
std=0.05
jump_prob=0.05
n_obs_list=c(1000,5000)
n_iter=100
lags=1


armatjump.sim = function(n_obs, c, ar, std, jump_prob,df=3)
{
  
  #initial data points is distributed N(0,1)
  points = rnorm(1,0,1)
  
  # we will generate more n points so as to remove the first point
  for (i in 1:n_obs){
    
    # error is t-student with df=3 distributed
    error_t = rt(1,df)
    lag = points[length(points)]
    z_t = rbinom(1,1,jump_prob)
    
    new_point = (c*(1-ar)+ar*lag)+(1-z_t)*(std*error_t)+z_t*(-3*c+2*std*error_t)
    points = c(points,new_point)
  }
  
  points = points[2:length(points)]
  return(points)
}


armatjump_simulator = function(n_obs, c, ar, std, jump_prob)
{
  data = armatjump.sim(n_obs, c, ar, std, jump_prob)
  data <- as.data.frame(data)
  colnames(data) = "y"
  return(data)
}



for (n_obs in n_obs_list) {
  
  this_simulator = partial(armatjump_simulator, n_obs=n_obs,
                            c=c, ar=ar, std=std, jump_prob=jump_prob)
  this_loss = simulation_run(this_simulator, lags=lags, n_iter = n_iter)
  
  this_pbloss  = this_loss$pbloss
  this_cdeloss = this_loss$cdeloss
  
  write_rds(this_pbloss, paste0("../results/PBLOSS_ARMATJUMP_",n_obs,"obs.rds"))
  write_rds(this_pbloss, paste0("../results/CDELOSS_ARMAJUMP_",n_obs,"obs.rds"))
  
  processed_loss = process_loss_outputs(this_pbloss,this_cdeloss)
  write_rds(processed_loss,paste0("../results/processed/ARMATJUMP_",n_obs,"obs.rds"))
}
