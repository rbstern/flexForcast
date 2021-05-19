#source("C:/Users/NAJA INFO/Documents/RIPPLE/FlexCodeTS/utils.R")
#devtools::install_github("tpospisi/NNKCDE/r")
#source('./auxiliary/utils.R')
library(NNKCDE)
options(scipen = 999)


NNKCDE.train = function(train_valid_test_sets,alpha_seq){
  
  ytrain=train_valid_test_sets$ytrain
  yvalid=train_valid_test_sets$yvalid
  ytest=train_valid_test_sets$ytest
  Xtrain=train_valid_test_sets$Xtrain
  Xvalid=train_valid_test_sets$Xvalid
  Xtest=train_valid_test_sets$Xtest
  
  ## Train
  obj <- NNKCDE$new(Xtrain, ytrain)

  # create h-grid
  dist = outer(ytrain,ytrain, `-`)
  dist = dist[lower.tri(dist)]
  dist = as.vector(dist)
  dist = abs(dist[!is.na(dist)])
  dist = sort(dist)
  
  len = length(dist)
  idx1 = round(0.01*len)
  idx2 = round(0.5*len)
  h_grid = seq(dist[idx1],dist[idx2],length=20)
  
  k_grid <- seq(1, 101, 5) #len=21
  
  ## Tune to minimum loss
  obj$tune(Xvalid, yvalid, k_grid = k_grid, h_grid = h_grid)
  
  n_grid = 1000
  minimo = min(ytrain) # ytrain ? correto?
  maximo = max(ytrain)
  #z_grid <- seq(minimo-2*abs(minimo),maximo+2*abs(maximo), length.out = n_grid)
  z_grid <- seq(minimo,maximo, length.out = n_grid)
  z_grid=matrix(z_grid, nrow=length(z_grid),ncol=1) 
  bin_length = z_grid[2]-z_grid[1]
  
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

      q = quantile(cdes[j,],alpha,z_grid)
      q_alpha = c(q_alpha,q)
      
    }
    
    q_list[[i]]=q_alpha
    
  }
  
  # adapt cde values so that histogram area sums up to 1
  for (i in 1:nrow(cdes)){ # normalizacao das cdes
    cdes[i,]=cdes[i,]/bin_length
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

nnkcde_training   = function(train_valid_test_sets, alpha_seq)
{
  nnkcde_output= NNKCDE.train(train_valid_test_sets, alpha_seq)
  z_grid=nnkcde_output$output$z_grid
  cdes=nnkcde_output$output$cdes
  this_pbloss = NNKCDE.pbloss(nnkcde_output$output)
  ytest=train_valid_test_sets$ytest
  this_cdeloss = cdeloss(ytest, z_grid, cdes)
  list(cdeloss = this_cdeloss, pbloss = this_pbloss)
}

############################################################################

# EXAMPLE OF USAGE

#nnkcde_output= NNKCDE.train(train_valid_test_sets,c(0.05,0.20,0.5,0.80,0.95))
#pb_losses = NNKCDE.pbloss(nnkcde_output$output)  
#z_grid=nnkcde_output$output$z_grid
#cdes=nnkcde_output$output$cdes
#nnkcde_cdeloss = cdeloss(ytest,z_grid,cdes)


