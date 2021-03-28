#devtools::install_github("tpospisi/NNKCDE/r")

source('./method_estimate/QAR.R')
source('./method_estimate/NNKCDE.R')
source('./method_estimate/GARCH.R')
source('./method_estimate/FLEXCODE.R')
source('./auxiliary/load_datasets.R')
source('./auxiliary/preprocess.R')
source('./auxiliary/utils.R')
source('./method_training.R')

library(rlang)
library(tidyverse)
library(quantreg) 
library(qpcR)
library(tseries)
library(NNKCDE)
library(rugarch)
library(FlexCoDE)

###############################################################

p_test=0.15
p_valid=0.1
p_train=1-p_test-p_valid
lags=10
L=10
n_cores_flexcode=4
params = list(p_test=p_test,p_valid=p_valid,p_train=1-p_test-p_valid,
              lags=lags,L=L,n_cores_flexcode=n_cores_flexcode)

dataset=load.dataset("AR1")
train_valid_test_sets = create_train_valid_test_sets(dataset,p_valid=p_valid,p_test=p_test,lags)
alpha_seq=c(0.05,0.20,0.50,0.80,0.95)

test_methods = function(train_valid_test_sets,alpha_seq,params){
  
  p_test=params$p_test
  p_valid=params$p_valid
  p_train=params$p_train
  lags=params$lags
  L=params$L
  n_cores_flexcode=params$n_cores_flexcode
  
  qar_training2 = partial(qar_training,L=L)
  flexcode_rf   = partial(flexcode_training,p_train=p_train,regressionFunction=regressionFunction.Forest,lags_y=lags,n_cores=n_cores_flexcode)
  flexcode_xgb  = partial(flexcode_training,p_train=p_train,regressionFunction=regressionFunction.XGBoost,lags_y=lags,n_cores=n_cores_flexcode)
  
  methods = list(qar_training2,nnkcde_training,garch_training,flexcode_rf,flexcode_xgb)
  nomes = c("QAR","NNKCDE","GARCH","FLEX-RF","FLEX-XGB")
  
  pb_losses = c()
  cde_losses = c()
  
  for(t_method in methods){
    aux = t_method(train_valid_test_sets,alpha_seq)
    pb_losses = cbind(pb_losses, aux$pbloss)
    cde_losses = cbind(cde_losses, aux$cdeloss)
  }
  
  df_pbloss=data.frame(pb_losses)
  colnames(df_pbloss) = nomes
  
  return(list(pbloss=df_pbloss,cdeloss=cde_losses))
}
 
#losses = test_methods(train_valid_test_sets,c(0.01,0.05,0.20,0.50,0.80,0.95,0.99),params)
#pblosses  = losses$pbloss
#cdelosses = losses$cdeloss
