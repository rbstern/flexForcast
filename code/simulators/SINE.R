n_iter=100
ar_param = 0.80
n_obs_list = c(1000,5000)
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
  
  write_rds(this_pbloss, paste0("../results/PBLOSS_SINE_AR_",n_obs,"obs.rds"))
  write_rds(this_cdeloss, paste0("../results/CDELOSS_SINE_AR_",n_obs,"obs.rds"))
  
  processed_loss = process_loss_outputs(this_pbloss,this_cdeloss)
  write_rds(processed_loss,paste0("../results/processed/SINE_AR_",n_obs,"obs.rds"))
  
}

#############################################################################

n_obs=1000
cdelosses = list()

for (lags in c(1,3,5,10,15,20,25,50,100)){
  
  this_simulator = partial(sine_ar_simulator,n_obs=n_obs)
  this_loss = simulation_run(this_simulator, lags=lags, n_iter = n_iter, mini=TRUE)
  this_cdeloss = this_loss$cdeloss
  cdelosses[[as.character(lags)]]=this_cdeloss
  
}

df = matrix(nrow=9,ncol=3)
for (i in 1:9){
  losses_i=colMeans(cdelosses[[i]])
  df[i,]=losses_i
}
colnames(df)=c("GARCH","NNK-CDE","FLEX-RF")
df=as.data.frame(df)

lmax = max(df)
lmin = min(df)
d = lmax-lmin

plot(c(1,3,5,10,15,20,25,50,100),df$GARCH,type='b',bty="l",xlab="No. of Lags",ylab="CDE loss",ylim=c(lmin-0.1*d,1),col=rgb(0.2,0.4,0.1,0.7) , lwd=3 , pch=17, xaxt="none")
lines(c(1,3,5,10,15,20,25,50,100),df$`NNK-CDE`,type="b",col=rgb(0.8,0.4,0.1,0.7) , lwd=3 , pch=19)
lines(c(1,3,5,10,15,20,25,50,100),df$`FLEX-RF`,type="b",col=rgb(0.8,0.7,0.1,0.7) , lwd=3 , pch=15)
axis(1, c(1,3,5,10,15,20,25,50,100),las=2, cex.axis=0.8, font=2)
legend("right", 
       legend = c("GARCH","NNK-CDE","FLEX-RF"), 
       col = c(rgb(0.2,0.4,0.1,0.7), 
               rgb(0.8,0.4,0.1,0.7),
               rgb(0.8,0.7,0.1,0.7)), 
       pch = c(17,19,15), 
       bty = "n", 
       pt.cex = 1.5, 
       cex = 0.6, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))
