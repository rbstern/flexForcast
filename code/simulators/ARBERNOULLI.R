n_iter=100
p_bernoulli = 0.85
ar_param = 0.80
n_obs_list = c(1000,5000)
lags=1

bernoulli_ar.sim = function(p_bernoulli,ar_param,n,var=FALSE){
  
  #initial data point is distributed N(0,1)
  points = rnorm(1,0,1)
  
  #bernoulli sample: X ~ ber(p)
  X_t = rbinom(1,1,p_bernoulli)
  
  # we will generate (n+order) points so as to remove the first (order) points
  for (i in 1:n){
    
    # Check if the variance is time-varying
    if (var==TRUE){
      sigma_i = sqrt(abs(points[length(points)]))
    } else {
      sigma_i = 1
    }
    
    error_t = rnorm(1,0,sigma_i)
    new_point = ar_param*X_t*points[length(points)]+error_t
    points = c(points,new_point)
  }
  
  points = points[2:length(points)]
  return(points)
}

bernoulli_simulator = function(p_bernoulli,ar_param,n_obs)
{
  data = bernoulli_ar.sim(p_bernoulli,ar_param,n_obs)
  data <- as.data.frame(data)
  colnames(data) = "y"
  return(data)
}

bernoulli_simulator_time_varying_variance = function(p_bernoulli,ar_param,n_obs)
{
  data = bernoulli_ar.sim(p_bernoulli,ar_param,n_obs,var=TRUE)
  data <- as.data.frame(data)
  colnames(data) = "y"
  return(data)
}

for (n_obs in n_obs_list) {
  
  this_simulator = partial(bernoulli_simulator,
                           p_bernoulli=p_bernoulli,
                           ar_param=ar_param,
                           n_obs=n_obs)
  this_loss = simulation_run(this_simulator, lags=lags, n_iter = n_iter)
  
  this_pbloss  = this_loss$pbloss
  this_cdeloss = this_loss$cdeloss
  
  write_rds(this_pbloss, paste0("../results/PBLOSS_BERNOULLI_",n_obs,"obs.rds"))
  write_rds(this_pbloss, paste0("../results/CDELOSS_BERNOULLI_",n_obs,"obs.rds"))
 
  processed_loss = process_loss_outputs(this_pbloss,this_cdeloss)
  write_rds(processed_loss,paste0("../results/processed/BERNOULLI_",n_obs,"obs.rds"))
   
}

###################################################################################

print('time varying variance AR-bernoulli')

for (n_obs in n_obs_list) {
  
  this_simulator = partial(bernoulli_simulator_time_varying_variance,
                           p_bernoulli=p_bernoulli,
                           ar_param=ar_param,
                           n_obs=n_obs)
  this_loss = simulation_run(this_simulator, lags=lags, n_iter = n_iter)
  
  this_pbloss  = this_loss$pbloss
  this_cdeloss = this_loss$cdeloss
  
  write_rds(this_pbloss, paste0("../results/PBLOSS_BERNOULLI_TIMEVARYING_",n_obs,"obs.rds"))
  write_rds(this_pbloss, paste0("../results/CDELOSS_BERNOULLI_TIMEVARYING_",n_obs,"obs.rds"))
  
  processed_loss = process_loss_outputs(this_pbloss,this_cdeloss)
  write_rds(processed_loss,paste0("../results/processed/BERNOULLI_TIMEVARYING_",n_obs,"obs.rds"))
  
}
