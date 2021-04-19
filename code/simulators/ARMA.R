n_iter=100
ar_coeffs=c(0.3,0.1,0.2)
ma_coeffs=c(0.2,0.15)
n_obs_list = c(1000,5000)
lags=max(length(param[[1]]),length(param[[2]]))

arma.sim = function(ar_coeffs,ma_coeffs,n,df=3,var=FALSE){
  
  ar_order = length(ar_coeffs)
  ma_order = length(ma_coeffs)
  max_order = max(ar_order,ma_order)
  
  #initial data points are distributed N(0,1)
  points = rnorm(max_order,0,1)
  errors = rnorm(max_order,0,1)
  
  # we will generate (n+max_order) points so as to remove the first (max_order) points
  for (i in 1:n){
    

    ar_part = crossprod(points[(length(points)-ar_order+1):length(points)],rev(ar_coeffs))
    ma_part = crossprod(errors[(length(errors)-ma_order+1):length(errors)],rev(ma_coeffs))
    
    if (var==TRUE){
      sigma_i = sqrt(abs(points[length(points)]))
    } else {
      sigma_i = 1
    }
    
    error_t = rnorm(1,0,sigma_i)
    new_point = ar_part+ma_part+error_t
    
    errors = c(errors,error_t)
    points = c(points,new_point)
  }
  
  points = points[(max_order+1):length(points)]
  return(points)
}


arma_simulator = function(n_obs, ar_coeffs,ma_coeffs)
{
  data = arma.sim(ar_coeffs,ma_coeffs,n_obs)
  data <- as.data.frame(data)
  colnames(data) = "y"
  return(data)
}

arma_simulator_time_varying_variance = function(n_obs, ar_coeffs,ma_coeffs)
{
  data = data = arma.sim(ar_coeffs,ma_coeffs,n_obs,var=TRUE)
  data <- as.data.frame(data)
  colnames(data) = "y"
  return(data)
}

##########################################################

print('constant variance')

for (n_obs in n_obs_list) {
  
  this_simulator = partial(arma_simulator,n_obs=n_obs,ar_coeffs=ar_coeffs,ma_coeffs=ma_coeffs)
  this_loss = simulation_run(this_simulator, lags=lags, n_iter = n_iter)
  
  this_pbloss  = this_loss$pbloss
  this_cdeloss = this_loss$cdeloss
  
  write_rds(this_pbloss, paste0("../results/PBLOSS_ARMA_11_",n_obs,"obs.rds"))
  write_rds(this_pbloss, paste0("../results/CDELOSS_ARMA_11_",n_obs,"obs.rds"))
  
  processed_loss = process_loss_outputs(this_pbloss,this_cdeloss)
  write_rds(processed_loss,paste0("../results/processed/ARMA_11_",n_obs,"obs.rds"))
}

###################################################################################

print('time varying variance ARMA')

for (n_obs in n_obs_list) {
  
  this_simulator = partial(arma_simulator_time_varying_variance,n_obs=n_obs,ar_coeffs=ar_coeffs,ma_coeffs=ma_coeffs)
  this_loss = simulation_run(this_simulator, lags=lags, n_iter = n_iter)
  
  this_pbloss  = this_loss$pbloss
  this_cdeloss = this_loss$cdeloss
  
  write_rds(this_pbloss, paste0("../results/PBLOSS_ARMA_TIMEVARYING_",n_obs,"obs.rds"))
  write_rds(this_pbloss, paste0("../results/CDELOSS_ARMA_TIMEVARYING_",n_obs,"obs.rds"))
  
  processed_loss = process_loss_outputs(this_pbloss,this_cdeloss)
  write_rds(processed_loss,paste0("../results/processed/ARMA_TIMEVARYING_",n_obs,"obs.rds"))
}
}



