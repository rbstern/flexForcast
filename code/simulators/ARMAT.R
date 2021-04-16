ar_coeffs=c(0.3,0.1,0.2)
ma_coeffs=c(0.2,0.15)
n_obs_list = c(1000,5000)
n_iter=100
lags=max(length(ar_coeffs),length(ma_coeffs))

t_student_arma.sim = function(ar_coeffs,ma_coeffs,n,df=3){
  
  ar_order = length(ar_coeffs)
  ma_order = length(ma_coeffs)
  max_order = max(ar_order,ma_order)
  
  #initial data points are distributed N(0,1)
  points = rnorm(max_order,0,1)
  errors = rt(3,df)
  
  # we will generate (n+max_order) points so as to remove the first (max_order) points
  for (i in 1:n){
    
    # t-distribution with df=3 degrees of freedom

    ar_part = crossprod(points[(length(points)-ar_order+1):length(points)],rev(ar_coeffs))
    ma_part = crossprod(errors[(length(errors)-ma_order+1):length(errors)],rev(ma_coeffs))
    
    error_t = rt(1,df)
    new_point = ar_part+ma_part+error_t
    
    errors = c(errors,error_t)
    points = c(points,new_point)
  }
  
  points = points[(max_order+1):length(points)]
  return(points)
}

armat_simulator = function(n_obs, ar_coeffs,ma_coeffs)
{
  data = t_student_arma.sim(ar_coeffs,ma_coeffs,n_obs)
  data <- as.data.frame(data)
  colnames(data) = "y"
  return(data)
}

for (n_obs in n_obs_list) {
  
  this_simulator = partial(armat_simulator,ar_coeffs=ar_coeffs,
                           ma_coeffs=ma_coeffs,n_obs=n_obs)
  this_loss = simulation_run(this_simulator, lags=lags, n_iter = n_iter)
  
  this_pbloss  = this_loss$pbloss
  this_cdeloss = this_loss$cdeloss
  
  write_rds(this_pbloss, paste0("../results/PBLOSS_ARMAT_",n_obs,"obs.rds"))
  write_rds(this_pbloss, paste0("../results/CDELOSS_ARMAT_",n_obs,"obs.rds"))
  
  processed_loss = process_loss_outputs(this_pbloss,this_cdeloss)
  write_rds(processed_loss,paste0("../results/processed/ARMAT_",n_obs,"obs.rds"))
}


