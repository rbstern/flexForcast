# k is a parameter for simulations of external generates dataset
# for a dataset that generated n observations, k will read the k-th observation
# from the full dataset

simulation_run = function(simulator, lags=0, n_iter = 100, read=NULL)
{
  
  for(i in 1:n_iter)
  {
    
    if (!is.null(read)){
      data = simulator(k=i)
    } else {
      data <- simulator()
    }
    
    
    train_valid_test_sets = create_train_valid_test_sets(dataset=data,lags=lags)
    
    print(paste("Iteration: ", i, "/", n_iter, sep = ""))
    loss <- test_methods(train_valid_test_sets,lags=lags)

    
    if (i==1){
      
      df_pbloss  = loss$pbloss
      df_cdeloss = loss$cdeloss
      i = i + 1
      
    } else {
      
      df_pbloss = rbind(df_pbloss,loss$pbloss)
      df_cdeloss = rbind(df_cdeloss,loss$cdeloss)
      
    }
  }
  
  CatchupPause(0.001)
  return(list(pbloss=df_pbloss,cdeloss=df_cdeloss))
}
