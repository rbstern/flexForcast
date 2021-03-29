coeffs=c(0.38,0.4,0.21)
n_obs_list = c(200,250)
n_iter=100

t_student_ar.sim = function(coeffs,n,df=3){
  
  order = length(coeffs)
  
  #initial data points are distributed N(0,1)
  points = rnorm(order,0,1)
  
  # we will generate (n+order) points so as to remove the first (order) points
  for (i in 1:n){
    
    # t-distribution with df=3 degrees of freedom
    error_t = rt(1,df)
    new_point = crossprod(points[(length(points)-order+1):length(points)],rev(coeffs))+error_t
    points = c(points,new_point)
  }
  
  points = points[(order+1):length(points)]
  return(points)
}



art_simulator = function(n_obs, coeffs)
{
  data = t_student_ar.sim(coeffs,n_obs)
  data <- as.data.frame(data)
  colnames(data) = "y"
  return(data)
}

for (n_obs in n_obs_list) {
  
  this_art_simulator = partial(art_simulator,n_obs=n_obs,coeffs=coeffs)
  this_loss = simulation_run(this_ar_simulator, n_iter = n_iter)
  
  this_pbloss  = this_loss$pbloss
  this_cdeloss = this_loss$cdeloss
  
  write_rds(this_pbloss, paste0("../results/PBLOSS_ART_3_",n_obs,"obs.rds"))
  write_rds(this_pbloss, paste0("../results/CDELOSS_ART_3_",n_obs,"obs.rds"))
  
}

