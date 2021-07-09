n_iter=100-
ar_param = 0.80
#n_obs_list = c(1000,2500,5000,10000)
n_obs_list = c(1000,2000)
lags=1

sine_ar.sim = function(n){
  
  #initial data point is distributed N(0,1)
  points = rnorm(1,0,1)
  sd <- 0.25
  
  # we will generate (n+order) points so as to remove the first (order) points
  for (i in 1:n){
    error_t = rnorm(1,0,sd)
    new_point = sin(pi*points[length(points)])^2+error_t
    points = c(points,new_point)
  }
  
  points = points[2:length(points)]
  return(points)
}

sine_ar_simulator = function(n_obs)
{
  data = sine_ar.sim(n_obs)
  data <- as.data.frame(data)
  colnames(data) = "y"
  return(data)
}


for (n_obs in n_obs_list) {
  
  this_simulator = partial(sine_ar_simulator,
                           n_obs=n_obs)
  this_loss = simulation_run(this_simulator, lags=lags, n_iter = n_iter)
  
  this_pbloss  = this_loss$pbloss
  this_cdeloss = this_loss$cdeloss
  
  write_rds(this_pbloss, paste0("../results/PBLOSS_SINEAR_",n_obs,"obs.rds"))
  write_rds(this_cdeloss, paste0("../results/CDELOSS_SINEAR_",n_obs,"obs.rds"))
  
  processed_loss = process_loss_outputs(this_pbloss,this_cdeloss)
  write_rds(processed_loss,paste0("../results/processed/SINEAR_",n_obs,"obs.rds"))
  gc()
}

#############################################################################
run=FALSE
if (run == TRUE){
  n_obs=1000
  cdelosses = list()
  
  for (lags in c(1,3,5,10,15,20,25,50,100)){
    
    this_simulator = partial(sine_ar_simulator,n_obs=n_obs)
    this_loss = simulation_run(this_simulator, lags=lags, n_iter = n_iter)
    this_cdeloss = this_loss$cdeloss
    cdelosses[[as.character(lags)]]=this_cdeloss
    
  }
  
  for (i in 1:length(cdelosses)){
    dfi = cdelosses[[i]]
    for (j in 1:ncol(cdelosses[[i]]))
      dfi[,j][is.na(dfi[,j])] <- max(dfi[,j], na.rm = TRUE)
    cdelosses[[i]] = dfi
  }
  
  df = matrix(nrow=6,ncol=3)
  dfse = matrix(nrow=6,ncol=3)
  for (i in 1:6){
    losses_i=colMeans(cdelosses[[i]])
    losses_se = apply(cdelosses[[i]], 2,sd)
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
  
  library(ggplot2)
  
  x= c(1,3,5,10,15,20,25,50,100)
  ggplot(data=df, aes(x=x))+
    geom_ribbon(aes(ymin=GARCH_low,ymax=GARCH_up),alpha=0.25)+
    geom_line(aes(y=GARCH,color="blue"),size=0.9)+
    geom_ribbon(aes(ymin=NNKCDE_low,ymax=NNKCDE_up),alpha=0.25)+
    geom_line(aes(y=NNKCDE,color="red"),size=0.9)+
    geom_ribbon(aes(ymin=FLEX_RF_low,ymax=FLEX_RF_up),alpha=0.25)+
    geom_line(aes(y=FLEX_RF,color="green"),size=0.9)+
    scale_color_discrete(name = "Methods", labels = c("GARCH","FLEXCODE-RF","NNK-CDE"))+
    ylab("CDE loss") + xlab("No. of lags") +
    scale_x_continuous(breaks=x)
}



# plot(c(1,3,5,10,15,20,25,50,100),df$GARCH,type='b',bty="l",xlab="No. of Lags",ylab="CDE loss",ylim=c(lmin-0.1*d,1),col=rgb(0.2,0.4,0.1,0.7) , lwd=3 , pch=17, xaxt="none")
# lines(c(1,3,5,10,15,20,25,50,100),df$`NNK-CDE`,type="b",col=rgb(0.8,0.4,0.1,0.7) , lwd=3 , pch=19)
# lines(c(1,3,5,10,15,20,25,50,100),df$`FLEX-RF`,type="b",col=rgb(0.8,0.7,0.1,0.7) , lwd=3 , pch=15)
# 
# axis(1, c(1,3,5,10,15,20,25,50,100),las=2, cex.axis=0.8, font=2)
# legend("right",
#        legend = c("GARCH","NNK-CDE","FLEX-RF"),
#        col = c(rgb(0.2,0.4,0.1,0.7),
#                rgb(0.8,0.4,0.1,0.7),
#                rgb(0.8,0.7,0.1,0.7)),
#        pch = c(17,19,15),
#        bty = "n",
#        pt.cex = 1.5,
#        cex = 0.6,
#        text.col = "black",
#        horiz = F ,
#        inset = c(0.1, 0.1))
