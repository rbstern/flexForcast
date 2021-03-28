#source("C:/Users/NAJA INFO/Documents/RIPPLE/FlexCodeTS/utils.R")
#devtools::install_github("tpospisi/NNKCDE/r")
library(NNKCDE)
options(scipen = 999)

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


NNKCDE.train = function(train_valid_test_sets,alpha_seq){
  
  ytrain=train_valid_test_sets$ytrain
  yvalid=train_valid_test_sets$yvalid
  ytest=train_valid_test_sets$ytest
  Xtrain=train_valid_test_sets$Xtrain
  Xvalid=train_valid_test_sets$Xvalid
  Xtest=train_valid_test_sets$Xtest
  
  ## Train
  obj <- NNKCDE$new(Xtrain, ytrain, h = 0.15)
  
  ## Estimate errors
  k_grid <- 1:10
  loss_list <- obj$estimate_loss(Xvalid, yvalid, k_grid = k_grid)
  
  ## Tune to minimum loss
  obj$tune(Xvalid, yvalid, k_grid = seq(1, 20, 100))
  
  ###############
  ## CONFIRMAR ##  
  ###############
  
  n_grid = 1000
  minimo = min(ytrain) # ytrain ? correto?
  maximo = max(ytrain)
  #z_grid <- seq(minimo-2*abs(minimo),maximo+2*abs(maximo), length.out = n_grid)
  z_grid <- seq(minimo,maximo, length.out = n_grid)
  z_grid=matrix(z_grid, nrow=length(z_grid),ncol=1) 
  
  cdes <- obj$predict(Xtest, z_grid)
  
  for (i in 1:nrow(cdes)){ # normalizacao das cdes
    cdes[i,]=cdes[i,]/sum(cdes[i,])
  }
  
  ######################
  
  q_list = list()
  
  for (i in 1:length(alpha_seq)){
    
    alpha = alpha_seq[i]
    q_alpha = c()
    
    for (j in 1:length(cdes[,1])){
      
      q_alpha = c(q_alpha,quantile(cdes[j,],alpha,z_grid))
    }
    
    q_list[[i]]=q_alpha
    
  }
  
  output = list(quantiles=q_list,alpha_seq=alpha_seq,ytest=ytest,z_grid=z_grid,cdes=cdes)
  
  return(list(output=output,nnk_model=obj))

}

NNKCDE.pbloss = function(NNKCDE_output){
  
  alpha_seq = NNKCDE_output$alpha_seq
  quantiles = NNKCDE_output$quantiles
  ytest     = NNKCDE_output$ytest
  
  results = c()
  
  for (i in 1:length(alpha_seq)){
    results = c(results, pinball_loss(ytest,quantiles[[i]],alpha_seq[i]))
  }
  
  return(results)
}

############################################################################

# EXAMPLE OF USAGE

#nnkcde_output= NNKCDE.train(train_valid_test_sets,c(0.05,0.20,0.5,0.80,0.95))
#pb_losses = NNKCDE.pbloss(nnkcde_output$output)  
#z_grid=nnkcde_output$output$z_grid
#cdes=nnkcde_output$output$cdes
#nnkcde_cdeloss = cdeloss(ytest,z_grid,cdes)


