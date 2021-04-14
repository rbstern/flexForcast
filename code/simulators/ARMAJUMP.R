c=0.1
ar=0.9
std=0.05
jump_prob=0.05
n_obs_list=c(1000,5000)
n_iter=100
lags=1


armajump.sim = function(n_obs, c, ar, std, jump_prob)
{
  
  #initial data points is distributed N(0,1)
  points = rnorm(1,0,1)
  
  # we will generate more n points so as to remove the first point
  for (i in 1:n_obs){
    
    # error is N(0,1) distributed
    error_t = rnorm(1,0,1)
    lag = points[length(points)]
    z_t = rbinom(1,1,jump_prob)
    
    new_point = (c*(1-ar)+ar*lag)+(1-z_t)*(std*error_t)+z_t*(-3*c+2*std*error_t)
    points = c(points,new_point)
  }
  
  points = points[2:length(points)]
  return(points)
}


armajump_simulator = function(n_obs, c, ar, std, jump_prob)
{
  data = armajump.sim(n_obs, c, ar, std, jump_prob)
  data <- as.data.frame(data)
  colnames(data) = "y"
  return(data)
}

# armajump_simulator = function(n_obs, data, k){
#   
#   if (n_obs!=1000 & n_obs!=5000){
#     stop('For this simulation, it is only possible to choose 1000 or 5000 observations.')
#   }
# 
#   this_data = t(data[k,2:(n_obs+1)])
#   this_data <- as.data.frame(this_data)
#   colnames(this_data)="y"
#   
#   return(this_data)
#   
# }

for (n_obs in n_obs_list) {
  
  #data <- read.csv(paste0('../data/arma_jump_simulations_',n_obs,'obs.csv'))
  
  #this_armajump_simulator = partial(armajump_simulator,n_obs=n_obs,data=data)
  #this_loss = simulation_run(this_armajump_simulator, n_iter = n_iter, read=TRUE)
  
  this_simulator = partial(armajump_simulator, n_obs=n_obs,
                            c=c, ar=ar, std=std, jump_prob=jump_prob)
  this_loss = simulation_run(this_simulator, lags=lags, n_iter = n_iter)
  
  this_pbloss  = this_loss$pbloss
  this_cdeloss = this_loss$cdeloss
  
  write_rds(this_pbloss, paste0("../results/PBLOSS_ARMAJUMP_",n_obs,"obs.rds"))
  write_rds(this_pbloss, paste0("../results/CDELOSS_ARMAJUMP_",n_obs,"obs.rds"))
  
  processed_loss = process_loss_outputs(this_pbloss,this_cdeloss)
  write_rds(processed_loss,paste0("../results/processed/ARMAJUMP_",n_obs,"obs.rds"))
}
