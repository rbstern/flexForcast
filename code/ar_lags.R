source("./set_environment.R")

n_iter=100
coeffs=c(0.2,0.3,0.35)
n_obs_list = c(1000,2500,5000)
lags=length(coeffs)

ar.sim = function(coeffs,n,df=3,var=FALSE){
  
  order = length(coeffs)
  
  #initial data points are distributed N(0,1)
  points = rnorm(order,0,1)
  
  # we will generate (n+order) points so as to remove the first (order) points
  for (i in 1:n){
    
    # Check if the variance is time-varying
    if (var==TRUE){
      sigma_i = sqrt(abs(points[length(points)]))
    } else {
      sigma_i = 1
    }
    
    error_t = rnorm(1,0,sigma_i)
    new_point = crossprod(points[(length(points)-order+1):length(points)],rev(coeffs))+error_t
    points = c(points,new_point)
  }
  
  points = points[(order+1):length(points)]
  return(points)
}

ar_simulator = function(n_obs, coeffs)
{
  data = ar.sim(coeffs=coeffs, n = n_obs)
  data <- as.data.frame(data)
  colnames(data) = "y"
  return(data)
}

ar_simulator_time_varying_variance = function(n_obs, coeffs)
{
  data = ar.sim(coeffs=coeffs, n = n_obs, var=TRUE)
  data <- as.data.frame(data)
  colnames(data) = "y"
  return(data)
}

#################################################################

library(ggplot2)

grid_lags <- c(1,3,5,10,15,30,50)

n_obs=1000
cdelosses = list()
pinballlosses = list()

for (lags in grid_lags){
  
  this_simulator = partial(ar_simulator,n_obs=n_obs,coeffs=coeffs)
  this_loss = simulation_run(this_simulator, lags=lags, n_iter = n_iter)
  cdelosses[[as.character(lags)]]=this_loss$cdeloss
  pinballlosses[[as.character(lags)]]=this_loss$pbloss
  
}

for (i in 1:length(cdelosses)){
  dfi = cdelosses[[i]]
  for (j in 1:ncol(cdelosses[[i]]))
    dfi[,j][is.na(dfi[,j])] <- max(dfi[,j], na.rm = TRUE)
  cdelosses[[i]] = dfi
}

df = matrix(nrow=length(grid_lags),ncol=3)
dfse = matrix(nrow=length(grid_lags),ncol=3)
for (i in 1:length(grid_lags)){
  losses_i=colMeans(cdelosses[[i]])
  losses_se = apply(cdelosses[[i]], 2,sd)/sqrt(nrow(cdelosses[[i]]))
  df[i,]=losses_i
  dfse[i,]=losses_se
}
colnames(df)=c('NNKCDE','GARCH','FLEX_RF')
colnames(dfse)=c('NNKCDEse','GARCHse','FLEX_RFse')
df = cbind(df,dfse)
df=as.data.frame(df)

df['GARCH_low']=df['GARCH']-df['GARCHse']
df['GARCH_up']=df['GARCH']+df['GARCHse']
df['NNKCDE_low']=df['NNKCDE']-df['NNKCDEse']
df['NNKCDE_up']=df['NNKCDE']+df['NNKCDEse']
df['FLEX_RF_low']=df['FLEX_RF']-df['FLEX_RFse']
df['FLEX_RF_up']=df['FLEX_RF']+df['FLEX_RFse']

ggplot(data=df, aes(x=grid_lags))+
  geom_ribbon(aes(ymin=GARCH_low,ymax=GARCH_up),alpha=0.25)+
  geom_line(aes(y=GARCH,color="blue"),size=0.9)+
  geom_ribbon(aes(ymin=NNKCDE_low,ymax=NNKCDE_up),alpha=0.25)+
  geom_line(aes(y=NNKCDE,color="red"),size=0.9)+
  geom_ribbon(aes(ymin=FLEX_RF_low,ymax=FLEX_RF_up),alpha=0.25)+
  geom_line(aes(y=FLEX_RF,color="green"),size=0.9)+
  scale_color_discrete(name = "Methods", labels = c("GARCH","FLEXCODE-RF","NNK-CDE"))+
  ylab("CDE loss") + xlab("No. of lags") +
  scale_x_continuous(breaks=grid_lags)

df$lags <- grid_lags
df_tidy_mean <- pivot_longer(df%>% dplyr::select(-ends_with("_up"))%>% 
                               dplyr::select(-ends_with("_low")),
                             cols=c("NNKCDE","FLEX_RF","GARCH"),
                             names_to = "method",values_to = "mean")%>% 
  dplyr::select(lags,method,mean) %>% 
  arrange(lags,method)

df_tidy_se <- pivot_longer(df%>% dplyr::select(-ends_with("_up"))%>% 
                             dplyr::select(-ends_with("_low")),
                           cols=ends_with("se"),
                           names_to = "method",
                           values_to = "se") %>% 
  dplyr::select(lags,method,se)%>% 
  mutate(method=recode(method,GARCHse="GARCH",FLEX_RFse="FLEX_RF",NNKCDEse="NNKCDE"))%>% 
  arrange(lags,method)


df_tidy <- left_join(df_tidy_mean,df_tidy_se,by=c("lags","method")) 

#saveRDS(df_tidy,"../results/lags/ar.RDS")
df_tidy=readRDS("../results/lags/ar.RDS")
library(tidyverse)

ggplot(df_tidy,aes(x=lags,y=mean,
                   ymin=mean-se,ymax=mean+se,fill=method))+
  geom_line(aes(color=method),size=1.2)+
  geom_ribbon(alpha=0.2)+
  theme_bw(base_size = 16)+
  #facet_grid(. ~ title)+
  scale_color_manual(name="",values=c("#000000", "#1b9e77", "#d95f02","#7570b3"))+
  scale_fill_manual(name="",values=c("#000000", "#1b9e77", "#d95f02","#7570b3"))+
  #scale_color_brewer(name = "",palette="Dark2")+ 
  theme(legend.position="top",
        axis.text.x = element_text(size=14),
        plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks=unique(df_tidy$lags))+
  #ggtitle(settings[this_setting])+
  xlab("Number of lags")+
  ylab("CDE Loss")
ggsave("../figures/loss_ar_lags.png",height = 7,width = 14)



lags <- 50
n_obs=1000
this_simulator = partial(ar_simulator,n_obs=n_obs,coeffs=coeffs)
data <- this_simulator()
train_valid_test_sets = create_train_valid_test_sets(dataset=data,lags=lags)
fit <- flexcode_training(train_valid_test_sets,alpha_seq,p_train=p_train+p_valid,
                             regressionFunction=regressionFunction.Forest,n_cores=4,
                         lags_y=lags,nIMax=50,chooseSharpen=TRUE)
  

ytrain=train_valid_test_sets$ytrain
yvalid=train_valid_test_sets$yvalid
ytest=train_valid_test_sets$ytest

ytrain = c(ytrain,yvalid)

Xtrain=train_valid_test_sets$Xtrain
Xvalid=train_valid_test_sets$Xvalid
Xtest=train_valid_test_sets$Xtest

Xtrain = rbind(Xtrain,Xvalid)

fit <- fit_flexcode_timeseries(X=Xtrain,y=ytrain,
                               lags_x=0,
                               lags_y=0,
                               nTrain=round((p_train+p_valid)*length(ytrain)),
                               regressionFunction=regressionFunction.Forest,
                               nIMax=5,chooseSharpen=TRUE,
                               regressionFunction.extra=list(nCores=5))


print.Forest=function(regressionObject,bestI,nameCovariates)
{
  
  if(length(regressionObject$fittedReg[[1]]$importance)==1)
  {
    importance=t(sapply(regressionObject$fittedReg,function(x)x$importance)[1:bestI])
    
  } else {
    importance=sapply(regressionObject$fittedReg,function(x)x$importance)[,1:bestI]
  }
  freq=rowMeans(importance)
  
  table=data.frame(covariate=order(freq,decreasing = FALSE),
                   frequency=sort(freq,decreasing =  FALSE))
  cat(paste("Average Importance of each covariate: \n"))
  print(table)
  
  
  if(is.null(nameCovariates))
    nameCovariates=1:nrow(table)
  
  table$covariate=factor(table$covariate,levels=table$covariate)
  ggplot2::ggplot(table, ggplot2::aes(x=as.factor(covariate),y=frequency))+
    ggplot2::geom_bar(position="dodge",stat="identity") +
    ggplot2::coord_flip()+ggplot2::ylab("Average Importance")+
    ggplot2::xlab("Lag")+
    ggplot2::scale_x_discrete(labels=nameCovariates[as.numeric(as.character(table$covariate))])+
    theme_bw(base_size = 16)
  
  
  
}
print.Forest(fit$cde_fit$regressionObject,
             fit$cde_fit$bestI,nameCovariates = paste(1:50))
ggsave("../figures/importance_ar_lags.png",height = 10,width = 7)
