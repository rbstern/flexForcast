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
