c=0.1
ar1=0.1
ar2=0.4
ar3=0.4
std=0.05
jump_prob=0.05
n_obs_list = c(5000)
#n_obs_list = c(1000,2500,5000)
n_iter=100
lags=3


armatjump.sim = function(n_obs, c, ar1, ar2, ar3, std, jump_prob, df=3)
{
  
  #initial data points is distributed N(0,1)
  points = rnorm(3,0,1)
  
  # we will generate more n points so as to remove the first point
  for (i in 1:n_obs){
    
    # error is N(0,1) distributed
    error_t = rt(1,df)
    lag1 = points[length(points)]
    lag2 = points[length(points)-1]
    lag3 = points[length(points)-2]
    z_t = rbinom(1,1,jump_prob)
    
    new_point = (c*(1-ar1-ar2-ar3)+ar1*lag1+ar2*lag2+ar3*lag3)+(1-z_t)*(std*error_t)+z_t*(-3*c+2*std*error_t)
    points = c(points,new_point)
  }
  
  points = points[4:length(points)]
  return(points)
}


armajump_simulator = function(n_obs, c, ar1, ar2, ar3, std, jump_prob)
{
  data = armatjump.sim(n_obs, c, ar1, ar2, ar3, std, jump_prob)
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
  
  this_simulator = partial(armajump_simulator, n_obs=n_obs, c=c,
                           ar1=ar1, ar2=ar2, ar3=ar3, std=std, jump_prob=jump_prob)
  this_loss = simulation_run(this_simulator, lags=lags, n_iter = n_iter)
  
  this_pbloss  = this_loss$pbloss
  this_cdeloss = this_loss$cdeloss
  
  write_rds(this_pbloss, paste0("../results/PBLOSS_ARMAJTUMP3_",n_obs,"obs.rds"))
  write_rds(this_pbloss, paste0("../results/CDELOSS_ARMATJUMP3_",n_obs,"obs.rds"))
  
  processed_loss = process_loss_outputs(this_pbloss,this_cdeloss)
  write_rds(processed_loss,paste0("../results/processed/ARMATJUMP3_",n_obs,"obs.rds"))
  
  gc()
}
