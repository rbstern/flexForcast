#source("./test_methods.R")

n_iter=100
param=c(0.2,0.3,0.35)
n_obs_list = c(1000,5000)
lags=length(param)

ar_simulator = function(n_obs, param)
{
  data = arima.sim(n = n_obs, list(ar = param))
  data <- as.data.frame(data)
  colnames(data) = "y"
  return(data)
}

for (n_obs in n_obs_list) {
  
  this_simulator = partial(ar_simulator,n_obs=n_obs,param=param)
  this_loss = simulation_run(this_simulator, lags=lags, n_iter = n_iter)
  
  this_pbloss  = this_loss$pbloss
  this_cdeloss = this_loss$cdeloss
  
  write_rds(this_pbloss, paste0("../results/full/PBLOSS_AR_1_",n_obs,"obs.rds"))
  write_rds(this_pbloss, paste0("../results/full/CDELOSS_AR_1_",n_obs,"obs.rds"))
  
  processed_loss = process_loss_outputs(this_pbloss,this_cdeloss)
  write_rds(processed_loss,paste0("../results/processed/AR_1_",n_obs,"obs.rds"))
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
