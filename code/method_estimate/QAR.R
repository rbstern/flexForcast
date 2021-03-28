options(digits = 4)
#install.packages('qpcR')
library(quantreg) # For quantile regression
library(qpcR)
library(tseries)


lagorder = function(x,maxorder){ # maxorder is the maximum lag number you allow
  ordmax = maxorder ; HQ = NULL ; AK = NULL ; SC = NULL ; lagmat = NULL
  l1 = length(x)
  for (i in 1:ordmax){
    lagmat = cbind(lagmat[-i,],x[(1):(l1-i)]) # lagged matrix
    armod <- lm(x[(i+1):l1]~lagmat)
    AK[i] = AIC(armod)
    #SC[i] = BIC(armod)
  }
  #return(c(which.min(AK),which.min(SC) ))
  return(which.min(AK))
}

lagmatrix <- function(x,max.lag) embed(c(rep(NA,max.lag), x), max.lag+1)

prepare_dataframe <- function(x,L,test=FALSE){

  #train
  #L = lagorder(x, maxorder = 24)
  lags = lagmatrix(x,L)

  if (!test){
    lags=data.frame(lags[(L+1):nrow(lags),])
  }

  columns = c('y')
  for (i in 1:L){
    columns = c(columns,paste('lag',i,sep=''))
  }
  colnames(lags)=columns

  return(lags)
}

QuantileAR <- function(df,tauseq){

  qr0 = list()
  for (i in 1:length(tauseq)){
    qr0[[i]] = rq(y~.,data = df, tau = tauseq[i])
  }

  quantiles=list()
  coefs = list()
  nx = ncol(df)-1

  for (i in 1:length(tauseq)){

      qs = c()
      B = qr0[[i]]$coef

      for (j in (nx+1):nrow(df)){
        x = c(1,df$y[(j-1):(j-nx)])
        q=sum(B*x)
        qs = c(qs,q)
      }

      coefs[[i]]=B
      quantiles[[i]]=qs

  }
  return(list(quantiles,coefs))
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

QAR.predict_quantiles = function(dftrain,dftest,qar_output,tauseq,L){

  df = tail(dftrain,L)
  df = rbind(df,dftest)
  rownames(df)=NULL

  quantiles = list()
  nx = ncol(df)-1

  for (i in 1:length(tauseq)){
    qs = c()
    B = qar_output[[2]][[i]]

    for (j in (nx+1):nrow(df)){
      x = c(1,df$y[(j-1):(j-nx)])
      q=sum(B*x)
      qs = c(qs,q)
    }

    pb_loss = pinball_loss(df$y[(nx+1):nrow(df)],qs,tauseq[i])

    qar_result = list()
    qar_result[[1]]=qs
    qar_result[[2]]=pb_loss

    quantiles[[i]]=qar_result
  }

  return(quantiles)
}

QAR.run = function(ytrain,y_new,tauseq,L=0){

  if (L==0){
    L = lagorder(ytrain, maxorder = 24)
    print(paste(L,"lags were selected."))
  }

  dftrain = prepare_dataframe(ytrain,L)
  dftest = prepare_dataframe(ytest,L,test=TRUE)
  qar = QuantileAR(dftrain,tauseq)
  qtest= QAR.predict_quantiles(dftrain,dftest,qar,tauseq,L)
  return(qtest)

}

QAR.plot = function(y,quantiles,tauseq){

  plot(y,type='l')
  lines(quantiles[[1]][[1]],col='red')
  lines(quantiles[[5]][[1]],col='blue')

  plot(y,type='l')
  lines(quantiles[[2]][[1]],col='red')
  lines(quantiles[[4]][[1]],col='blue')

  plot(y,type='l')
  lines(quantiles[[3]][[1]],col='red')

  for (i in 1:length(tauseq)){
    print(paste("The pinball loss of",tauseq[i],"quantile was",quantiles[[i]][[2]]))
  }

}

#qs = QAR.run(ytrain,ytest,c(0.05,0.20,0.50,0.80,0.95),10)
