simulation_run = function(simulator, n_iter = 100)
{
  
  for(i in 1:n_iter)
  {
    data <- simulator()
    train_valid_test_sets = create_train_valid_test_sets(dataset=data)
    
    print(paste("Iteration: ", i, "/", n_iter, sep = ""))
    loss <- test_methods(train_valid_test_sets)

    
    if (i==1){
      
      df_pbloss  = loss$pbloss
      df_cdeloss = loss$cdeloss
      i = i + 1
      
    } else {
      
      df_pbloss = rbind(df_pbloss,loss$pbloss)
      df_cdeloss = rbind(df_cdeloss,loss$cdeloss)
      
    }
  }
  
  return(list(pbloss=df_pbloss,cdeloss=df_cdeloss))
}
