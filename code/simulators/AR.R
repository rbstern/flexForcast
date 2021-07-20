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

print('constant variance')

for (n_obs in n_obs_list) {
  
  this_simulator = partial(ar_simulator,n_obs=n_obs,coeffs=coeffs)
  this_loss = simulation_run(this_simulator, lags=lags, n_iter = n_iter)
  
  this_pbloss  = this_loss$pbloss
  this_cdeloss = this_loss$cdeloss
  
  write_rds(this_pbloss, paste0("../results/full/PBLOSS_AR_3_",n_obs,"obs.rds"))
  write_rds(this_pbloss, paste0("../results/full/CDELOSS_AR_3_",n_obs,"obs.rds"))
  
  processed_loss = process_loss_outputs(this_pbloss,this_cdeloss)
  write_rds(processed_loss,paste0("../results/processed/AR_3_",n_obs,"obs.rds"))
  
  gc()
}

###################################################################################

# print('time varying variance AR')
# 
# for (n_obs in n_obs_list) {
#   
#   this_simulator = partial(ar_simulator_time_varying_variance,n_obs=n_obs,coeffs=coeffs)
#   this_loss = simulation_run(this_simulator, lags=lags, n_iter = n_iter)
#   
#   this_pbloss  = this_loss$pbloss
#   this_cdeloss = this_loss$cdeloss
#   
#   write_rds(this_pbloss, paste0("../results/full/PBLOSS_AR_3_TIMEVARYING_",n_obs,"obs.rds"))
#   write_rds(this_pbloss, paste0("../results/full/CDELOSS_AR_3_TIMEVARYING_",n_obs,"obs.rds"))
#   
#   processed_loss = process_loss_outputs(this_pbloss,this_cdeloss)
#   write_rds(processed_loss,paste0("../results/processed/AR_3_TIMEVARYING_",n_obs,"obs.rds"))
# }

#####################################################################################

run=TRUE
library(ggplot2)

grid_lags <- c(1,3,5,10,15,30,50)

if (run==TRUE){
  
  n_obs=1000
  cdelosses = list()
  pinballlosses = list()
  
  for (lags in grid_lags){
    
    this_simulator = partial(ar_simulator,n_obs=n_obs,coeffs=coeffs)
    this_loss = simulation_run(this_simulator, lags=lags, n_iter = n_iter)
    cdelosses[[as.character(lags)]]=this_loss$cdeloss
    pinballlosses[[as.character(lagpblosss)]]=this_loss$pbloss
    
  }
  saveRDS(pinballlosses,"../results/lags/pinball_ar.RDS")
  
  for (i in 1:length(cdelosses)){
    dfi = cdelosses[[i]]
    for (j in 1:ncol(cdelosses[[i]]))
      dfi[,j][is.na(dfi[,j])] <- max(dfi[,j], na.rm = TRUE)
    cdelosses[[i]] = dfi
  }
  

  df = matrix(nrow=length(grid_lags),ncol=3)
  dfse = matrix(nrow=length(grid_lags),ncol=3)
  dfpb95 = matrix(nrow=length(grid_lags),ncol=4)
  dfpbse95 = matrix(nrow=length(grid_lags),ncol=4)
  dfpb80 = matrix(nrow=length(grid_lags),ncol=4)
  dfpbse80 = matrix(nrow=length(grid_lags),ncol=4)
  dfpb50 = matrix(nrow=length(grid_lags),ncol=4)
  dfpbse50 = matrix(nrow=length(grid_lags),ncol=4)
  for (i in 1:length(grid_lags)){
    processed_loss = process_loss_outputs(pinballlosses[[i]],cdelosses[[i]])
    df[i,]=processed_loss$cdeloss$mean
    dfse[i,]=processed_loss$cdeloss$se
    dfpb95[i,]=processed_loss$pbloss$mean[5,-1]
    dfpbse95[i,]=processed_loss$pbloss$se[5,-1]
    dfpb80[i,]=processed_loss$pbloss$mean[4,-1]
    dfpbse80[i,]=processed_loss$pbloss$se[4,-1]
    dfpb50[i,]=processed_loss$pbloss$mean[3,-1]
    dfpbse50[i,]=processed_loss$pbloss$se[3,-1]
  }
  colnames(df)=c('NNKCDE','GARCH','FLEX_RF')
  colnames(dfse)=c('NNKCDEse','GARCHse','FLEX_RFse')
  colnames(dfpb95)=colnames(processed_loss$pbloss$mean)[-1]
  colnames(dfpbse95)=paste0(colnames(processed_loss$pbloss$mean)[-1],"se")
  colnames(dfpb80)=colnames(processed_loss$pbloss$mean)[-1]
  colnames(dfpbse80)=paste0(colnames(processed_loss$pbloss$mean)[-1],"se")
  colnames(dfpb50)=colnames(processed_loss$pbloss$mean)[-1]
  colnames(dfpbse50)=paste0(colnames(processed_loss$pbloss$mean)[-1],"se")
  df = cbind(df,dfse)
  df=as.data.frame(df)
  dfpb95 = cbind(dfpb95,dfpbse95)
  dfpb95=as.data.frame(dfpb95)
  dfpb80 = cbind(dfpb80,dfpbse80)
  dfpb80=as.data.frame(dfpb80)
  dfpb50 = cbind(dfpb50,dfpbse50)
  dfpb50=as.data.frame(dfpb50)
  
  df['GARCH_low']=df['GARCH']-df['GARCHse']
  df['GARCH_up']=df['GARCH']+df['GARCHse']
  df['NNKCDE_low']=df['NNKCDE']-df['NNKCDEse']
  df['NNKCDE_up']=df['NNKCDE']+df['NNKCDEse']
  df['FLEX_RF_low']=df['FLEX_RF']-df['FLEX_RFse']
  df['FLEX_RF_up']=df['FLEX_RF']+df['FLEX_RFse']
}



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
#ggsave("../figures/loss_ar_lags.png",height = 7,width = 14)





dfpb95$lags <- grid_lags
dfpb80$lags <- grid_lags
dfpb50$lags <- grid_lags
dfpb95$Quantile <- "95%"
dfpb80$Quantile <- "80%"
dfpb50$Quantile <- "50%"
dfpb <- rbind(dfpb50,dfpb80,dfpb95)

dfpb_tidy_mean <- pivot_longer(dfpb%>% dplyr::select(-ends_with("_up"))%>% 
                               dplyr::select(-ends_with("_low")),
                             cols=c("QAR","NNKCDE","FLEX_RF","GARCH"),
                             names_to = "method",values_to = "mean")%>% 
  dplyr::select(lags,method,mean,Quantile) %>% 
  arrange(lags,method)

dfpb_tidy_se <- pivot_longer(dfpb%>% dplyr::select(-ends_with("_up"))%>% 
                             dplyr::select(-ends_with("_low")),
                           cols=ends_with("se"),
                           names_to = "method",
                           values_to = "se") %>% 
  dplyr::select(lags,method,se,Quantile)%>% 
  mutate(method=recode(method,QARse="QAR",GARCHse="GARCH",FLEX_RFse="FLEX_RF",NNKCDEse="NNKCDE"))%>% 
  arrange(lags,method)


dfpb_tidy <- left_join(dfpb_tidy_mean,dfpb_tidy_se,by=c("lags","method","Quantile")) 
#saveRDS(dfpb_tidy,"../results/lags/arpb.RDS")


ggplot(dfpb_tidy,aes(x=lags,y=mean,
                   ymin=mean-se,ymax=mean+se,fill=method))+
  geom_line(aes(color=method),size=1.2)+
  geom_ribbon(alpha=0.2)+
  theme_bw(base_size = 16)+
  facet_grid(. ~ Quantile)+
  scale_color_manual(name="",values=c("#000000", "#1b9e77", "#d95f02","#7570b3"))+
  scale_fill_manual(name="",values=c("#000000", "#1b9e77", "#d95f02","#7570b3"))+
  #scale_color_brewer(name = "",palette="Dark2")+ 
  theme(legend.position="top",
        axis.text.x = element_text(size=14),
        plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks=unique(dfpb_tidy$lags))+
  #ggtitle(settings[this_setting])+
  xlab("Number of lags")+
  ylab("Quantile Loss")
#ggsave("../figures/loss_ar_lags_pb.png",height = 7,width = 14)
