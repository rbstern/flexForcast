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
