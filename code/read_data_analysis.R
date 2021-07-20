plot_cde=function(fit,nameCovariates)
{
  regressionObject=fit$regressionObject
  bestI=fit$bestI
  if(length(regressionObject$fittedReg[[1]]$importance$Gain)==1)
  {
    importance=t(sapply(regressionObject$fittedReg,function(x)x$importance$Gain)[1:(bestI-1)])
    
  } else {
    importance=sapply(regressionObject$fittedReg,function(x)x$importance$Gain)[,1:(bestI-1),drop=FALSE]
  }
  freq=rowMeans(importance)
  if(!is.null(regressionObject$names.covariates))
  {
    freq=freq[match(regressionObject$fittedReg[[1]]$importance$Feature,
                    nameCovariates)]
  } else {
    freq=freq[order(regressionObject$fittedReg[[1]]$importance$Feature)]
  }
  
  
  table=data.frame(covariate=order(freq,decreasing = FALSE),
                   frequency=sort(freq,decreasing =  FALSE))
  cat(paste("Average Importance of each covariate: \n"))
  print(table)
  
  
  if(is.null(nameCovariates))
    nameCovariates=1:nrow(table)
  
  table$covariate=factor(table$covariate,levels=table$covariate)
  ggplot2::ggplot(table, ggplot2::aes(x=as.factor(covariate),y=frequency))+
    ggplot2::geom_bar(position="dodge",stat="identity") +
    ggplot2::coord_flip()+ggplot2::ylab("Average Importance")+ggplot2::xlab("Covariate")+
    ggplot2::scale_x_discrete(labels=nameCovariates[as.numeric(as.character(table$covariate))])+
  theme_bw(base_size = 16)
  
  
}




library(tidyverse)
library(FlexCoDE)
X <- read_csv("../data/x_train_nome_colunas.csv")
X <- X[,-1]
Y <- read_csv("../data/y_train_nome_colunas.csv")

train <- 1:round(0.7*nrow(X))

set.seed(0)
fit <- fitFlexCoDE(xTrain = X[train,],zTrain = Y[train,2],
                   xValidation =  X[-train,],zValidation = Y[-train,2],
                   regressionFunction = regressionFunction.XGBoost,nIMax = 50)
plot_cde(fit,fit$covariateNames)
ggsave("../figures/real_data_importance.png",height = 10,width = 7)
