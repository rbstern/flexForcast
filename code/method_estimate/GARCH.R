

GARCH.run=function(ytrain,yeval,ytest,alpha_seq,lags){
  
  try({
    model.garch = ugarchspec(mean.model=list(armaOrder=c(lags,lags)),
                             variance.model=list(garchOrder=c(lags,lags)),
                             distribution.model = "std")
    

    
    model.garch.fit = ugarchfit(data=c(ytrain,yeval,ytest), spec=model.garch,  solver = 'hybrid' ,out.sample = length(ytest))
    modelfor=ugarchforecast(model.garch.fit, data = ytest, n.ahead = 1,n.roll = length(ytest))
    print(2)
    quantiles_list = list()
    pred=modelfor@forecast$seriesFor[1,1:length(ytest)]
    
    for (ii in 1:length(alpha_seq)){
      q_garch_ii = qnorm(alpha_seq[ii],
                         mean=modelfor@forecast$seriesFor[1,1:length(ytest)],
                         sd=modelfor@forecast$sigmaFor[1,1:length(ytest)])
      quantiles_list[[ii]]=q_garch_ii
    }
    
  }
,silent = TRUE)
  if (!exists("modelfor")) {

    fit=auto.arima(c(ytrain,yeval),max.p = lags,max.q =lags)
    fit2=Arima(ytest,model=fit)
    
    quantiles_list = list()
    #pred=modelfor@forecast$seriesFor[1,1:length(ytest)]
    
    for (ii in 1:length(alpha_seq)){
      q_garch_ii = qnorm(alpha_seq[ii],
                         mean=fit2$fitted[(length(c(ytrain,yeval))+1):length(c(ytrain,yeval,ytest))],
                         sd=fit2$sigma2**(1/2))
      quantiles_list[[ii]]=q_garch_ii
    }
    
  }
  
  convergence_garch=exists("modelfor")
  
  #plot=matplot(data.frame(ztest,q_garch_05,q_garch_20,q_garch_80,q_garch_95),type = "l")
  
  n_grid = 1000
  z_grid = seq(min(ytrain), max(ytrain), length.out = n_grid)
  z_grid=matrix(z_grid, nrow=length(z_grid),ncol=1)
  
  df_quantiles = data.frame(quantiles_list)
  colnames(df_quantiles)=alpha_seq
  if (convergence_garch){
    return(list(df_quantiles=df_quantiles,garch_model = model.garch.fit,z_grid=z_grid,convergence_garch=convergence_garch))
  } else{
    return(list(df_quantiles=df_quantiles,garch_model = fit2,z_grid=z_grid,convergence_garch=convergence_garch))
  }
  }

GARCH.pinball_loss = function(garch_output,ytest,alpha_seq){
  
  df = garch_output$df_quantiles
  results = c()
  
  for (i in 1:length(alpha_seq)){
    results = c(results, pinball_loss(ytest,df[,i],alpha_seq[i]))
  }
  
  return(results)
  
}

GARCH.cde_estimate = function(garch_output,ytest){
  
  z_grid = garch_output$z_grid
  
  if (garch_output$convergence_garch) {
    
    garch_model = garch_output$garch_model
    modelfor=ugarchforecast(garch_model, data = ytest, n.ahead = 1,n.roll = length(ytest))
    mean_vector = modelfor@forecast$seriesFor[1,1:length(ytest)]
    sigma_vector = modelfor@forecast$sigmaFor[1,1:length(ytest)]
    
  } else {
    print("sem convergencia")
    mean_vector=garch_output$fit2$fitted[(length(garch_output$fit2$fitted)-length(ytest)+1):length(garch_output$fit2$fitted)]
    sigma_vector=rep(garch_output$fit2$sigma2**(1/2),length(ytest))
    
  }
  
  n_grid=1000
  cdes = matrix(0,nrow=length(ytest),ncol=n_grid)
  
  for (i in 1:length(ytest)){
    
    cdes[i,] = dnorm(z_grid,mean = mean_vector[i],sd=sigma_vector[i])
    cdes[i,] = cdes[i,]/sum(cdes[i,])
  }
  
  return(cdes)
}

garch_training = function(train_valid_test_sets,alpha_seq,lags)
{
  ytrain=train_valid_test_sets$ytrain
  yvalid=train_valid_test_sets$yvalid
  ytest=train_valid_test_sets$ytest
  garch_output = GARCH.run(ytrain,yvalid,ytest,alpha_seq,lags)
  cdes = GARCH.cde_estimate(garch_output,ytest)
  
  cde_loss_garch = cdeloss(ytest,garch_output$z_grid,cdes)
  garch_loss = GARCH.pinball_loss(garch_output,ytest,alpha_seq)
  
  list(cdeloss = cde_loss_garch, pbloss = garch_loss)
}

