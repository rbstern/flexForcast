quantiles <- function(pred_values,
                      percentiles=seq(0.05,0.95,length.out = 20))
{
  quantiles <- apply(pred_values$CDE,1,function(x)
  {
    x <- cumsum(x/sum(x))
    quantiles_vec <- apply(as.matrix(percentiles),1,function(p)
    {
      return(pred_values$z[which.min(x<=p)])
    })
    return(quantiles_vec)
  })
  
  return(t(quantiles))
}

quantile = function(cde,alpha,z_grid){
  
  CDF = cumsum(cde)/sum(cde)
  idx = which.min(abs(CDF-alpha))
  
  if (CDF[1]>alpha){
    q_alpha = z_grid[1]
    
  } else if (CDF[idx] > alpha){
    
    a = z_grid[idx-1]
    b = z_grid[idx]
    Fa = CDF[idx-1]
    Fb = CDF[idx]
    q_alpha = ((b-a)*(alpha-Fa)/(Fb-Fa))+a
    
  } else {
    a = z_grid[idx]
    b = z_grid[idx+1]
    Fa = CDF[idx]
    Fb = CDF[idx+1]
    q_alpha = ((b-a)*(alpha-Fa)/(Fb-Fa))+a
  }
  
  return(q_alpha)
}

cdeloss <- function(z_test,z_grid,pred) {
  
  z_grid <- as.matrix(z_grid)
  
  z_min <- apply(z_grid, 2, min)
  z_max <- apply(z_grid, 2, max)
  z_delta <- prod(z_max - z_min) / nrow(z_grid)
  pred=pred/rowSums(pred)
  pred=pred/z_delta
  
  integrals <- z_delta * sum(pred ^ 2) / nrow(pred)
  
  nn_ids <- cbind(1:length(z_test), FNN::knnx.index(z_grid, z_test, k = 1))
  likeli <- mean(pred[nn_ids])
  
  return(integrals - 2 * likeli)
  
}

pinball_loss=function(y,y_predicted,alpha){
  diff = y-y_predicted
  mask = y > y_predicted
  loss=(alpha*sum(diff[mask])-(1-alpha)*sum(diff[!mask]))/length(y_predicted)
  return(loss)
}

crosses = function(y,yq){
  compare = y > yq
  count = 0
  for (j in 2:length(y)){
    if (compare[j]!=compare[j-1]){
      count = count + 1
    }
  }
  return(count/(length(y)-1))
}

mat2_3d2 <- function(inmat, n) {
  if (nrow(inmat) %% n != 0) stop("incompatible dimensions provided")
  dims <- c(nrow(inmat) %/% n, ncol(inmat), n)
  array(unlist(split(inmat, gl(n, dims[1])), use.names = FALSE), dim = dims)
}

process_loss_outputs = function(pbloss,cdeloss){
  
  #PBLOSS
  pbloss_array=mat2_3d2(pbloss,n=n_iter)
  
  pbloss_mean = apply(pbloss_array, c(1,2), mean)
  pbloss_mean = cbind(quantile=alpha_seq,pbloss_mean)
  colnames(pbloss_mean)[2:6] = c('QAR','NNKCDE','GARCH','FLEX_RF','FLEX_XGB')
  
  pbloss_se  = apply(pbloss_array, c(1,2), sd)
  pbloss_se = pbloss_se/sqrt(n_iter)
  pbloss_se = cbind(quantile=alpha_seq,pbloss_se)
  colnames(pbloss_se)[2:6] = c('QAR','NNKCDE','GARCH','FLEX_RF','FLEX_XGB')
  
  pb = list(mean=pbloss_mean,se=pbloss_se)
  
  #cdeloss
  cdeloss_mean = colMeans(cdeloss)
  cdeloss_se = apply(cdeloss, 2, sd)
  cdeloss_se = cdeloss_se/sqrt(n_iter)
  
  cdeloss_mean = matrix(cdeloss_mean,nrow = 1, ncol = 4)
  colnames(cdeloss_mean)=c('NNKCDE','GARCH','FLEX_RF','FLEX_XGB')
  cdeloss_se = matrix(cdeloss_se,nrow = 1, ncol = 4)
  colnames(cdeloss_se)=c('NNKCDE','GARCH','FLEX_RF','FLEX_XGB')
  
  cd = list(mean=cdeloss_mean,se=cdeloss_se)
  
  return(list(pbloss=pb,cdeloss=cd))
  
}

CatchupPause <- function(Secs){
  Sys.sleep(Secs) #pause to let connection work
  closeAllConnections()
  gc()
}

final_loss_comparations = function(alpha_seq){
  
  n_simulations = length(list.files('../results/processed/'))
  n_losses = 1 + length(alpha_seq)
  
  cde_table = matrix(0,nrow=n_simulations*2,ncol=4)
  pb_list = list()
  
  for (alpha in alpha_seq){
    pb_list[[as.character(alpha)]]=matrix(0,nrow=n_simulations*2,ncol=5)
  }
  
  idx=1
  
  for (file in list.files('../results/processed/')){
    
    results = readRDS(paste0('../results/processed/',file))
    
    cde_mean = results$cdeloss$mean
    cde_se   = results$cdeloss$se
    cde_info = rbind(cde_mean,cde_se)
    cde_table[(2*idx-1):(2*idx),] = cde_info
    
    for (i in 1:length(alpha_seq)){
      
      pb_mean_i = results$pbloss$mean[1,2:6]
      pb_se_i   = results$pbloss$se[1,2:6]
      pb_info_i = rbind(pb_mean_i,pb_se_i)
      pb_list[[i]][(2*idx-1):(2*idx),] = pb_info_i
      
    }
    
    idx = idx+1
    
  }
  
  tables= list()
  tables[['cdeloss']] = cde_table
  
  k=1
  for (alpha in alpha_seq){
    tables[[as.character(alpha)]] = pb_list[[k]]
    k = k+1
  }
  
  return(tables)
  
}

for ()