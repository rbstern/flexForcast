n_iter=100
ar_param = 0.80
#n_obs_list = c(1000,2500,5000)
#n_obs_list = c(5000)
n_obs_list = c(1000,2500)
lags=3

sine_ar.sim = function(n){
  
  #initial data point is distributed N(0,1)
  points = rnorm(3,0,1)
  sd <- 0.25
  
  # we will generate (n+order) points so as to remove the first (order) points
  for (i in 3:n){
    error_t = rnorm(1,0,sd)
    new_point = sin(pi*points[length(points)-2])^2+error_t
    points = c(points,new_point)
  }
  
  points = points[4:length(points)]
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
  
  write_rds(this_pbloss, paste0("../results/PBLOSS_SINElagged_",n_obs,"obs.rds"))
  write_rds(this_cdeloss, paste0("../results/CDELOSS_SINElagged_",n_obs,"obs.rds"))
  
  processed_loss = process_loss_outputs(this_pbloss,this_cdeloss)
  write_rds(processed_loss,paste0("../results/processed/SINElagged_",n_obs,"obs.rds"))
  gc()
}

#######################################################################

run=TRUE

if (run == TRUE){
  
  n_obs=1000
  cdelosses = list()
  
  grid_lags <- c(1,3,5,10,15,20,25,50)
  
  for (lags in grid_lags){
    
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
  
}

library(ggplot2)


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

saveRDS(df_tidy,"../results/lags/sine_lagged.RDS")



ggplot(df_tidy,aes(x=lags,y=mean,
                   ymin=mean-se,ymax=mean+se,fill=method))+
  geom_line(aes(color=method),size=1.2)+
  geom_ribbon(alpha=0.2)+
  theme_bw(base_size = 12)+
  #facet_grid(. ~ title)+
  scale_color_manual(name="",values=c("#000000", "#1b9e77", "#d95f02","#7570b3"))+
  scale_fill_manual(name="",values=c("#000000", "#1b9e77", "#d95f02","#7570b3"))+
  #scale_color_brewer(name = "",palette="Dark2")+ 
  theme(legend.position="top",
        axis.text.x = element_text(size=6),
        plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks=unique(lags))+
  #ggtitle(settings[this_setting])+
  xlab("Number of lags")+
  ylab("CDE Loss")

