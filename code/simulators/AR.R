n_iter=100
coeffs=c(0.2,0.3,0.35)
n_obs_list = c(1000,5000)
lags=length(coeffs)

ar.sim = function(coeffs,n,df=3,var=FALSE){
  
  order = length(coeffs)
  
  #initial data points are distributed N(0,1)
  points = rnorm(order,0,1)
  
  # we will generate (n+order) points so as to remove the first (order) points
  for (i in 1:n){
    
    # Check if the variance is time-varying
    if (var==TRUE){
      sigma_i = sqrt(abs(points[length(points)]))
    } else {
      sigma_i = 1
    }
    
    error_t = rnorm(1,0,sigma_i)
    new_point = crossprod(points[(length(points)-order+1):length(points)],rev(coeffs))+error_t
    points = c(points,new_point)
  }
  
  points = points[(order+1):length(points)]
  return(points)
}

ar_simulator = function(n_obs, coeffs)
{
  data = ar.sim(coeffs=coeffs, n = n_obs)
  data <- as.data.frame(data)
  colnames(data) = "y"
  return(data)
}

ar_simulator_time_varying_variance = function(n_obs, coeffs)
{
  data = ar.sim(coeffs=coeffs, n = n_obs, var=TRUE)
  data <- as.data.frame(data)
  colnames(data) = "y"
  return(data)
}

#################################################################

print('constant variance')

for (n_obs in n_obs_list) {
  
  this_simulator = partial(ar_simulator,n_obs=n_obs,param=param)
  this_loss = simulation_run(this_simulator, lags=lags, n_iter = n_iter)
  
  this_pbloss  = this_loss$pbloss
  this_cdeloss = this_loss$cdeloss
  
  write_rds(this_pbloss, paste0("../results/full/PBLOSS_AR_3_",n_obs,"obs.rds"))
  write_rds(this_pbloss, paste0("../results/full/CDELOSS_AR_3_",n_obs,"obs.rds"))
  
  processed_loss = process_loss_outputs(this_pbloss,this_cdeloss)
  write_rds(processed_loss,paste0("../results/processed/AR_3_",n_obs,"obs.rds"))
}

###################################################################################

print('time varying variance AR')

for (n_obs in n_obs_list) {
  
  this_simulator = partial(ar_simulator_time_varying_variance,n_obs=n_obs,coeffs=coeffs)
  this_loss = simulation_run(this_simulator, lags=lags, n_iter = n_iter)
  
  this_pbloss  = this_loss$pbloss
  this_cdeloss = this_loss$cdeloss
  
  write_rds(this_pbloss, paste0("../results/full/PBLOSS_AR_3_TIMEVARYING_",n_obs,"obs.rds"))
  write_rds(this_pbloss, paste0("../results/full/CDELOSS_AR_3_TIMEVARYING_",n_obs,"obs.rds"))
  
  processed_loss = process_loss_outputs(this_pbloss,this_cdeloss)
  write_rds(processed_loss,paste0("../results/processed/AR_3_TIMEVARYING_",n_obs,"obs.rds"))
}

#'CDE_NNKCDE','CDE_FC_RF','CDE_XGB','CDE_GARCH'

#PBLOSS
# pbloss_array=mat2_3d2(this_pbloss,n=n_iter)
# loss_mean = apply(loss_array, c(1,2), mean)
# loss_mean = cbind(quantile=alpha_seq,loss_mean)
# colnames(loss_mean)[2:6] = c('QAR','NNKCDE','FLEX_RF','FLEX_XGB','GARCH')
# 
# loss_se  = apply(loss_array, c(1,2), sd)
# loss_se = loss_se/sqrt(n_iter)
# 
# #cdeloss
# cdeloss_mean = colMeans(this_cdeloss)
# cdeloss_se = apply(this_cdeloss, 2, sd)
# cdeloss_se = cdeloss_se/sqrt(n_iter)
# 
# cdeloss_mean = matrix(cdeloss_mean,nrow = 1, ncol = 4)
# colnames(cdeloss_mean)=c('NNKCDE','FLEX_RF','FLEX_XGB','GARCH')
# cdeloss_se = matrix(cdeloss_se,nrow = 1, ncol = 4)
# colnames(cdeloss_se)=c('NNKCDE','FLEX_RF','FLEX_XGB','GARCH')
