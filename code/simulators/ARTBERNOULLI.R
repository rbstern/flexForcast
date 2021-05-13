n_iter=100
p_bernoulli = 0.85
ar_param = 0.80
n_obs_list = c(2500,10000)
lags=1

bernoulli_art.sim = function(p_bernoulli,ar_param,n,df=3){
  
  #initial data point is distributed N(0,1)
  points = rnorm(1,0,1)
  
  #bernoulli sample: X ~ ber(p)
  X_t = rbinom(1,1,p_bernoulli)
  
  # we will generate (n+order) points so as to remove the first (order) points
  for (i in 1:n){
    
    # ERROR ~ N(0,1)
    error_t = rt(1,df)
    new_point = ar_param*X_t*points[length(points)]+error_t
    points = c(points,new_point)
  }
  
  points = points[2:length(points)]
  return(points)
}

tbernoulli_simulator = function(p_bernoulli,ar_param,n_obs)
{
  data = bernoulli_art.sim(p_bernoulli,ar_param,n_obs)
  data <- as.data.frame(data)
  colnames(data) = "y"
  return(data)
}

for (n_obs in n_obs_list) {
  
  this_simulator = partial(tbernoulli_simulator,
                           p_bernoulli=p_bernoulli,
                           ar_param=ar_param,
                           n_obs=n_obs)
  this_loss = simulation_run(this_simulator, lags=lags, n_iter = n_iter)
  
  this_pbloss  = this_loss$pbloss
  this_cdeloss = this_loss$cdeloss
  
  write_rds(this_pbloss, paste0("../results/PBLOSS_TBERNOULLI_",n_obs,"obs.rds"))
  write_rds(this_pbloss, paste0("../results/CDELOSS_TBERNOULLI_",n_obs,"obs.rds"))
  
  processed_loss = process_loss_outputs(this_pbloss,this_cdeloss)
  write_rds(processed_loss,paste0("../results/processed/TBERNOULLI_",n_obs,"obs.rds"))
  
}
