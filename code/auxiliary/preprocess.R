# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

create_design_matrix <- function(X=NULL,y,lags_x=NULL,
                                 lags_y=lags_x) {
  if(is.null(X)) # no covariates
  {
    if(lags_y==0)
      stop("need to use at least one lag for y if there are no covariates X")
    X_new <- matrix(Hmisc::Lag(y,shift=1))
    colnames(X_new)[ncol(X_new)] <-paste0("Y","_lag_",1)
    if(lags_y>1)
    {
      for(ii in 2:lags_y)
      {
        y_lag <- Hmisc::Lag(y,shift=ii)
        X_new <- cbind(X_new,y_lag)
        colnames(X_new)[ncol(X_new)] <-paste0("Y","_lag_",ii)
      }
      colnames(X_new)[1]<-paste0("Y","_lag_",1)
    }
  } else {
    if(!is.matrix(X))
      X <- as.matrix(X)
    X_new <- X
    colnames(X_new) <- paste0("X",1:ncol(X),"_lag_",0)
    if(lags_x>0)
    {
      for(ii in 1:lags_x)
      {
        X_lag <- apply(X,2,Hmisc::Lag,shift=ii)
        colnames(X_lag) <- paste0("X",1:ncol(X),"_lag_",ii)
        X_new <- cbind(X_new,X_lag)
      }
    }
    if(lags_y>0)
    {
      for(ii in 1:lags_y)
      {
        y_lag <- Hmisc::Lag(y,shift=ii)
        #if(is.vector(y_lag))
        #  y_lag <- t(y_lag)
        X_new <- cbind(X_new,y_lag)
        colnames(X_new)[ncol(X_new)] <-paste0("Y","_lag_",ii)
      }
    }
  }
  return(X_new)
}

fit_flexcode_timeseries <-function(X=NULL,y,lags_x=NULL,
                                   lags_y=lags_x,nTrain=round(0.8*length(y)),
                                   ...)
{
  y <- as.vector(y)
  if(is.null(lags_x)&is.null(lags_y))
    stop("Provide at least one lag value for either x or y")

  return_value <- list(lags_x=lags_x,
                       lags_y=lags_y,
                       X_train=X,
                       y_train=y)

  X_design <- create_design_matrix(X=X,y=y,
                                   lags_x=lags_x,
                                   lags_y=lags_y)

  # aqui temos que deslocar o y e dps remover as linhas de X_design
  # relativas a y's que sao NA

  which_complete <- complete.cases(X_design)
  X_design <- X_design[which_complete,]
  y <- y[which_complete]


  #  random_index <- sample(1:length(y),
  #                         size = length(y),
  #                        replace = TRUE)
  random_index <- 1:length(y)
  fit <- fitFlexCoDE(xTrain = X_design[random_index[1:nTrain],],
                     zTrain=y[random_index[1:nTrain]],
                     xValidation = X_design[random_index[-c(1:nTrain)],],
                     zValidation=y[random_index[-c(1:nTrain)]],
                     ...)
  return_value$cde_fit=fit
  class(return_value) <- "fit_flexcode_timeseries"
  return(return_value)
}


predict.fit_flexcode_timeseries <- function(fit,X_new=NULL,y_new=NA,
                                            predictionBandProb=FALSE)
{
  # X_new contains the covariates for each new instance since the last observation used for training
  # y_new contains the response for each new instance since the last observation used for training
  #   (it has one last element than the number of rows of X_new) y_new is NA if nrow(X_new)=1

  if(!is.na(y_new[length(y_new)]))
    y_new <- c(y_new,NA) # last y_new is the one that will be estimated

  if(is.null(X_new)) # no covariates
  {

  } else {

    if(nrow(X_new)==1)
    {
      if(!is.na(y_new))
        stop("y_new must be NA if X_new contains only one observation")
    }

    X_new_aug <- rbind(fit$X_train,X_new)

    y_new_aug <- c(fit$y_train,y_new)

    X_design_new <- create_design_matrix(X=X_new_aug,
                                         y=y_new_aug,
                                         lags_x=fit$lags_x,
                                         lags_y=fit$lags_y)
  }

  pred <- predict(fit$cde_fit,
                  X_design_new[nrow(X_design_new),,drop=FALSE],
                  predictionBandProb=predictionBandProb)
  pred_norm <- pred$CDE[1,]/sum(pred$CDE[1,])
  which_nn <- FNN::get.knnx(cumsum(pred_norm),
                            c((1-predictionBandProb)/2,
                              predictionBandProb+(1-predictionBandProb)/2),
                            k=1)$nn.index
  pred$pred_interval <- pred$z[which_nn]
  return(pred)
}

predict_experiments <- function(fit,X_new=NULL,y_new=NULL,
                                predictionBandProb=0.95)
{
  # aux predict function for experiments (cv like test)


  pred_values <- predict(fit,X_new[1,,drop=FALSE],
                         predictionBandProb=predictionBandProb)
  estimated_densities <- matrix(NA,nrow(X_new),length(pred_values$z))
  th <- rep(NA,nrow(X_new))
  lower <- rep(NA,nrow(X_new))
  upper <- rep(NA,nrow(X_new))
  estimated_densities[1,] <- pred_values$CDE
  th[1] <- pred_values$th
  lower[1] <- pred_values$pred_interval[1]
  upper[1] <- pred_values$pred_interval[2]
  for(ii in 2:nrow(X_new))
  {
    pred_values <- predict(fit,X_new[1:ii,,drop=FALSE],
                           y_new = y_new[1:(ii-1)],
                           predictionBandProb=predictionBandProb)
    estimated_densities[ii,] <- pred_values$CDE
    th[ii] <- pred_values$th
    lower[ii] <- pred_values$pred_interval[1]
    upper[ii] <- pred_values$pred_interval[2]
  }
  return(list(z=pred_values$z,
              CDE=estimated_densities,
              th=th,lower=lower,
              upper=upper))
}


plot.fit_flexcode_timeseries <- function(fit,X_new,
                                         y_new,predictionBandProb=predictionBandProb,
                                         y_train=NULL)
{
  pred_values <- predict_experiments(fit,X_new=X_new,
                                     y_new=y_new,
                                     predictionBandProb = predictionBandProb)

  data=data.frame(x=pred_values$z,
                  y=pred_values$CDE[1,],
                  dataPoint=rep(1,length(pred_values$z)),
                  vertical=y_new[1])
  for(i in 2:length(y_new))
  {
    dataB=data.frame(x=pred_values$z,
                     y=pred_values$CDE[i,],
                     dataPoint=rep(i,length(pred_values$z)),
                     vertical=y_new[i])
    data=rbind(data,dataB)
  }
  g=ggplot(data,ggplot2::aes(x=x,y=y))+
    geom_line(size=1,color=2)+xlab("Response")+
    ylab("Estimated Density")+
    geom_vline(ggplot2::aes(xintercept=vertical),size=1)+
    theme(axis.title=ggplot2::element_text(size=2,face="bold"))+
    facet_wrap(~ dataPoint)+
    theme_bw()
  print(g)

  eps=0.35
  lineWidthPred=1.5
  k=length(y_new)

  if(!is.null(y_train))
  {
    plot(x=1:length(c(y_train,y_new)),y=c(y_train,y_new),
         main="",ylab="Prediction Region",
         cex.main=1.4,cex.axis=1.4,cex.lab=1.4,cex=1.5,col=1,xaxt="n",
         xlim=c(0.5,length(c(y_train,y_new))+0.5),pch=16,
         ylim=c(fit$cde_fit$zMin,fit$cde_fit$zMax),xlab="Time",bty="l")
    for(ii in 1:k)
    {
      whichLarger=pred_values$CDE[ii,]>pred_values$th[ii]
      runs=rle(whichLarger>0)
      nRuns=length(runs$values)

      cumulative=cumsum(runs$lengths)
      for(jj in 1:nRuns)
      {
        if(runs$values[jj]==TRUE)
        {
          if(jj==1)
          {
            lower=fit$cde_fit$zMin
            upper=pred_values$z[cumulative[jj]]
            lines(c(ii+length(y_train),ii+length(y_train)),
                  c(lower,upper),col=1,lwd=lineWidthPred)
            lines(c(ii-eps+length(y_train),ii+eps+length(y_train)),
                  c(lower,lower),col=1,lwd=lineWidthPred)
            lines(c(ii-eps+length(y_train),ii+eps+length(y_train)),
                  c(upper,upper),col=1,lwd=lineWidthPred)
            next;
          }
          #points(rep(ii,sum(whichLarger)),predicted$z[whichLarger],pch=18,cex=0.9,col=2)
          lower=pred_values$z[cumulative[jj-1]]
          upper=pred_values$z[cumulative[jj]]
          lines(c(ii+length(y_train),ii+length(y_train)),
                c(lower,upper),col=1,lwd=lineWidthPred)

          lines(c(ii-eps+length(y_train),ii+eps+length(y_train)),
                c(lower,lower),col=1,lwd=lineWidthPred)
          lines(c(ii-eps+length(y_train),ii+eps+length(y_train)),
                c(upper,upper),col=1,lwd=lineWidthPred)
        }
      }
    }

    points(x=1:length(c(y_train,y_new)),y=c(y_train,y_new),main="",
           ylab="Estimate",cex.main=1.4,cex.axis=1.4,cex.lab=1.4,
           cex=1.5,col=2,xaxt="n",xlim=c(0.5,k+0.5),pch=16,
           xlab="Time")



    plot(x=1:length(c(y_train,y_new)),y=c(y_train,y_new),
         main="",ylab="Prediction Region",
         cex.main=1.4,cex.axis=1.4,cex.lab=1.4,cex=1.5,col=1,xaxt="n",
         xlim=c(0.5,length(c(y_train,y_new))+0.5),pch=16,
         ylim=c(fit$cde_fit$zMin,fit$cde_fit$zMax),xlab="Time",bty="l")
    for(ii in 1:k)
    {
      lower=pred_values$lower[ii]
      upper=pred_values$upper[ii]
      lines(c(ii+length(y_train),ii+length(y_train)),
            c(lower,upper),col=1,lwd=lineWidthPred)
      lines(c(ii-eps+length(y_train),ii+eps+length(y_train)),
            c(lower,lower),col=1,lwd=lineWidthPred)
      lines(c(ii-eps+length(y_train),ii+eps+length(y_train)),
            c(upper,upper),col=1,lwd=lineWidthPred)

    }
  }

  points(x=1:length(c(y_train,y_new)),y=c(y_train,y_new),main="",
         ylab="Estimate",cex.main=1.4,cex.axis=1.4,cex.lab=1.4,
         cex=1.5,col=2,xaxt="n",xlim=c(0.5,k+0.5),pch=16,
         xlab="Time")

  return()
}

estimate_volatility <- function(pred_values)
{
  volatility <- rep(NA,nrow(pred_values$CDE))
  for(ii in 1:nrow(pred_values$CDE))
  {
    probs <- pred_values$CDE[ii,]/sum(pred_values$CDE[ii,])
    volatility[ii] <- sum(probs*pred_values$z^2)-sum(probs*pred_values$z)^2
  }
  return(volatility)
}


coverage <- function(fit,X_new=X_new,
                     y_new=y_new,
                     percentiles=seq(0.05,0.95,length.out = 20))
{
  covered_th <- matrix(NA,nrow(X_new),length(percentiles))
  covered_quantile <- matrix(NA,nrow(X_new),length(percentiles))
  for(ii in seq_along(percentiles))
  {
    pred_values <- predict_experiments(fit,X_new=X_new,
                                       y_new=y_new,
                                       predictionBandProb = percentiles[ii])
    which_nn <- FNN::get.knnx(pred_values$z,y_new,k=1)$nn.index
    covered_th[,ii] <- pred_values$CDE[cbind(1:length(which_nn),which_nn)]>=pred_values$th
    covered_quantile[,ii] <- (pred_values$z[which_nn]>=pred_values$lower)&
      (pred_values$z[which_nn]<=pred_values$upper)
  }

  empirical_th <- colMeans(covered_th)
  plot(percentiles,empirical_th,xlim=c(0,1),ylim=c(0,1),
       pch=18,col=4,cex=3,xlab="Theoretical",ylab = "Empirical",
       main=paste("HPD",
                  class(fit)),cex.lab=1.4)
  abline(a=0,b=1,lwd=1.5)

  empirical_quantile <- colMeans(covered_quantile)
  plot(percentiles,empirical_quantile,xlim=c(0,1),ylim=c(0,1),
       pch=18,col=4,cex=3,xlab="Theoretical",ylab = "Empirical",
       main=paste("Quantile",
                  class(fit)),cex.lab=1.4)
  abline(a=0,b=1,lwd=1.5)

  return(list(empirical_th=empirical_th,
              empirical_quantile=empirical_quantile,
              theoretical=percentiles))
}



cde_loss_time <- function(fit,X_new,
                          y_new) {

  pred_values <- predict_experiments(fit,X_new=X_new,
                                     y_new=y_new)

  z_grid <- as.matrix(pred_values$z)
  pred <- pred_values$CDE
  z_test <- y_new

  z_min <- apply(z_grid, 2, min)
  z_max <- apply(z_grid, 2, max)
  z_delta <- prod(z_max - z_min) / nrow(z_grid)

  integrals <- z_delta * sum(pred ^ 2) / nrow(pred)

  nn_ids <- cbind(1:length(z_test), FNN::knnx.index(z_grid, z_test, k = 1))
  likeli <- mean(pred[nn_ids])

  return(integrals - 2 * likeli)
}

fit_kernel_timeseries <- function(X=Xtrain,y=ytrain,
                                  lags_x=3,
                                  lags_y=3)
{
  y <- as.vector(y)
  return_value <- list(lags_x=lags_x,
                       lags_y=lags_y,
                       X_train=X,
                       y_train=y)


  X_design <- create_design_matrix(X=X,y=y,
                                   lags_x=lags_x,
                                   lags_y=lags_y)

  which_complete <- complete.cases(X_design)
  X_design <- X_design[which_complete,]
  y <- y[which_complete]

  return_value$X_design <- X_design
  return_value$y_design <- y
  bw <- npcdensbw(xdat=X_design,
                  y=y,bwmethod="normal-reference")

  return_value$cde_fit=bw
  return_value$cde_fit$zMin=min(y)
  return_value$cde_fit$zMax=max(y)
  class(return_value) <- "fit_kde_timeseries"
  return(return_value)

}



predict.fit_kde_timeseries <- function(fit,X_new=NULL,y_new=NA,
                                       predictionBandProb=0.95)
{
  # X_new contains the covariates for each new instance since the last observation used for training
  # y_new contains the response for each new instance since the last observation used for training
  #   (it has one last element than the number of rows of X_new) y_new is NA if nrow(X_new)=1

  if(!is.na(y_new[length(y_new)]))
    y_new <- c(y_new,NA) # last y_new is the one that will be estimated

  if(is.null(X_new)) # no covariates
  {

  } else {

    if(nrow(X_new)==1)
    {
      if(!is.na(y_new))
        stop("y_new must be NA if X_new contains only one observation")
    }

    X_new_aug <- rbind(fit$X_train,X_new)

    y_new_aug <- c(fit$y_train,y_new)

    X_design_new <- create_design_matrix(X=X_new_aug,
                                         y=y_new_aug,
                                         lags_x=fit$lags_x,
                                         lags_y=fit$lags_y)
  }
  B <- 1000
  zMin=min(fit$y_train)
  zMax=max(fit$y_train)
  z_grid <- seq(zMin,
                zMax,
                length.out=B)
  predictions <- npcdens(bws=fit$cde_fit,
                         txdat=fit$X_design,
                         tydat = as.matrix(fit$y_design),
                         exdat=matrix(X_design_new[nrow(X_design_new),,drop=FALSE],
                                      length(z_grid),ncol(X_design_new),byrow = TRUE),
                         eydat=as.matrix(z_grid))

  pred <-list(z=z_grid,
              CDE=matrix(predictions$condens,1,length(z_grid)))
  pred_norm <- pred$CDE[1,]/sum(pred$CDE[1,])
  which_nn <- FNN::get.knnx(cumsum(pred_norm),
                            c((1-predictionBandProb)/2,
                              predictionBandProb+(1-predictionBandProb)/2),
                            k=1)$nn.index
  pred$pred_interval <- pred$z[which_nn]


  th <- matrix(NA, nrow(pred$CDE), 1)
  for (ii in 1:nrow(pred$CDE)) {
    th[ii] = FlexCoDE:::.findThresholdHPD((zMax - zMin)/B,
                                          pred$CDE[ii,], predictionBandProb)
  }
  pred$th <- th
  return(pred)
}

