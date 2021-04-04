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
  
  if (CDF[idx] > alpha){
    
    a = z_grid[idx-1]
    b = z_grid[idx]
    Fa = CDF[idx-1]
    Fb = CDF[idx]
    
  } else {
    a = z_grid[idx]
    b = z_grid[idx+1]
    Fa = CDF[idx]
    Fb = CDF[idx+1]
  }
  
  q_alpha = ((b-a)*(alpha-Fa)/(Fb-Fa))+a
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
  loss=(alpha*sum(diff[mask])-(1-alpha)*sum(diff[!mask]))/length(y)
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
  colnames(pbloss_mean)[2:6] = c('QAR','NNKCDE','FLEX_RF','FLEX_XGB','GARCH')
  
  pbloss_se  = apply(pbloss_array, c(1,2), sd)
  pbloss_se = pbloss_se/sqrt(n_iter)
  pbloss_se = cbind(quantile=alpha_seq,pbloss_se)
  colnames(pbloss_se)[2:6] = c('QAR','NNKCDE','FLEX_RF','FLEX_XGB','GARCH')
  
  pb = list(mean=pbloss_mean,se=pbloss_se)
  
  #cdeloss
  cdeloss_mean = colMeans(cdeloss)
  cdeloss_se = apply(cdeloss, 2, sd)
  cdeloss_se = cdeloss_se/sqrt(n_iter)
  
  cdeloss_mean = matrix(cdeloss_mean,nrow = 1, ncol = 4)
  colnames(cdeloss_mean)=c('NNKCDE','FLEX_RF','FLEX_XGB','GARCH')
  cdeloss_se = matrix(cdeloss_se,nrow = 1, ncol = 4)
  colnames(cdeloss_se)=c('NNKCDE','FLEX_RF','FLEX_XGB','GARCH')
  
  cd = list(mean=cdeloss_mean,se=cdeloss_se)
  
  return(list(pbloss=pb,cdeloss=cd))
  
}

CatchupPause <- function(Secs){
  Sys.sleep(Secs) #pause to let connection work
  closeAllConnections()
  gc()
}