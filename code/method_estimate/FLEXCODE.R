flexcode_training = function(train_valid_test_sets,alpha_seq,p_train,
                             regressionFunction,lags_y, n_cores=1,
                             lags_x=0,nIMax=50,chooseSharpen=TRUE)
{
  
  ytrain=train_valid_test_sets$ytrain
  ytest=train_valid_test_sets$ytest
  Xtrain=train_valid_test_sets$Xtrain
  Xtest=train_valid_test_sets$Xtest
  
  fit <- fit_flexcode_timeseries(X=Xtrain,y=ytrain,
                                 lags_x=lags_x,
                                 lags_y=lags_y,
                                 nTrain=round(p_train*length(ytrain)),
                                 regressionFunction=regressionFunction,
                                 nIMax=nIMax,chooseSharpen=chooseSharpen,
                                 regressionFunction.extra=list(nCores=n_cores))
  
  pred_values <- predict_experiments(fit,X_new=Xtest,y_new=ytest)
  fitted_quantiles <- quantiles(pred_values,percentiles = alpha_seq)
  pblosses = c()
  
  for (i in 1:length(alpha_seq)){
    pblosses = c(pblosses,pinball_loss(ytest,fitted_quantiles[,i],alpha_seq[i]))
  }
  
  z_grid = pred_values$z
  z_grid=matrix(z_grid, nrow=length(z_grid),ncol=1) 
  cdes = pred_values$CDE
  for (i in 1:length(ytest)){
    cdes[i,] = cdes[i,]/sum(cdes[i,])
  }
  cde_loss = cdeloss(ytest,z_grid,cdes)
  
  list(cdeloss = cde_loss, pbloss = pblosses)
}