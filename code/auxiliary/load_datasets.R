
load.dataset= function(name){

  if (name=='EUROSTOXX'){

    data <- read_csv("../data/eurostoxx_logreturns.csv")$x
    data <- as.data.frame(data)
    colnames(data)="y"

  } else if (name == 'BITCOIN'){

    data <- read_csv("../data/BTCUSD_Candlestick_1_M_BID_01.07.2020-01.08.2020.csv")$Close

    # select a subset of the series
    data=log(data[-1]/data[-length(data)])
    data=data[1:2000]
    data=(data-min(data))/(max(data)-min(data))

    data <- as.data.frame(data)
    colnames(data)="y"

  } else if (name == 'AR1') {

    data <- read_csv("../data/AR1.csv")$`0`
    data <- as.data.frame(data)
    colnames(data)="y"

  } else if (name == 'AR3') {

    data <- read.csv('../data/AR3.csv')$X0
    data <- as.data.frame(data)
    colnames(data)="y"

  } else if (name == 'ARMA') {

    data <- read.csv('../data/ARMA1_1.csv')$X0
    data <- as.data.frame(data)
    colnames(data)="y"

  } else if (name == 'ARMA JUMP') {

    data <- read.csv('../data/armajump.csv')$X0
    data <- as.data.frame(data)
    colnames(data)="y"

  } else if (name == 'JUMP DIFFUSION'){

    data <- read.csv('../data/jump_diffusion.csv')[,-1]
    colnames(data)[4] = "y" #log-returns

  } else if (name == 'CPI'){

    data <- read.csv('../data/USA_data.csv')[,-1]
    colnames(data)[1]="date"
    colnames(data)[2]="y"
    data$date = as.Date(data$date)

  } else if (name == 'RPIX') {

    data <- read.csv('../data/RPIX.csv')[,-1]
    colnames(data)[2]="y"
    data$year = substr(data$DATE,1,4)
    data$month = rep(c("01","02","03","04","05","06","07","08","09","10","11","12"),length.out = nrow(data))
    data$DATE = paste(data$year,'-',data$month,'-01',sep='')
    data$DATE=as.character(data$DATE)
    data$DATE=as.Date(data$DATE)
    data = data[,-c(3,4)]
    colnames(data)[1]="date"

  } else {
    print("Could not load data. This dataset name does not seem to be available.")
    data=0
  }

  return(data)

}

lagmatrix <- function(x,max.lag) embed(c(rep(NA,max.lag), x), max.lag+1)

create_train_valid_test_sets = function(dataset,p_valid,p_test,lags=0,remove=0){

  # p: proportion of train sample

  Xtrain = NULL
  Xvalid = NULL
  Xtest = NULL

  # no covariates
  if (ncol(dataset)==1) {
    # you want to add lags as covariates
    if (lags > 0){

      n_train = ceiling((1-p_test-p_valid)*(nrow(dataset)))
      n_valid = ceiling(p_valid*(nrow(dataset)))

      ytrain=dataset[(lags+1):(lags+n_train),]
      yvalid = dataset[(lags+n_train+1):(lags+n_train+n_valid),]
      ytest = dataset[(lags+n_train+n_valid+1):nrow(dataset),]

      X = data.frame(lagmatrix(dataset$y,lags))
      X = data.frame(X[,-1])

      columns = c( )
      for (i in 1:lags){
        columns = c(columns,paste('lag',i,sep=''))
      }
      
      colnames(X)=columns

      Xtrain = X[(lags+1):(lags+n_train),]
      Xvalid = X[(lags+n_train+1):(lags+n_train+n_valid),]
      Xtest = X[(lags+n_train+n_valid+1):nrow(dataset),]

    # if you dont need to add lags
    } else {

      n_train = ceiling((1-p_test-p_valid)*(nrow(dataset)-remove))
      n_valid = ceiling(p_valid*(nrow(dataset)-remove))

      ytrain=dataset[(remove+1):(remove+n_train),]
      yvalid = dataset[(remove+n_train+1):(remove+n_train+n_valid),]
      ytest = dataset[(remove+n_train+n_valid+1):nrow(dataset),]

    }

  # IF there are covariates

  } else {

    n_train = ceiling((1-p_test-p_valid)*(nrow(dataset)))
    n_valid = ceiling(p_valid*nrow(dataset))

    ytrain=dataset$y[1:n_train]
    yvalid = dataset$y[(n_train+1):(n_train+n_valid)]
    ytest = dataset$y[(n_train+n_valid+1):nrow(dataset)]

    X = dataset[,-which(names(dataset) == "y")]
    Xtrain = X[1:n_train,]
    Xvalid = X[(n_train+1):(n_train+n_valid),]
    Xtest = X[(n_train+n_valid+1):nrow(X),]

  }

  return(list(ytrain=ytrain,yvalid=yvalid,ytest=ytest,
              Xtrain=Xtrain,Xvalid=Xvalid,Xtest=Xtest))
}

# EXAMPLE OF LOADING DATASETS

# # univariate series with lags
# dataset=load.dataset("BITCOIN")
# train_valid_test_sets = create_train_valid_test_sets(dataset,0.1,0.15,lags=10)
# 
# ytrain=train_valid_test_sets$ytrain
# yvalid=train_valid_test_sets$yvalid
# ytest=train_valid_test_sets$ytest
# Xtrain=train_valid_test_sets$Xtrain
# Xvalid=train_valid_test_sets$Xtrain
# Xtest=train_valid_test_sets$Xtest
# 
# # ADD NO LAGS
# train_valid_test_sets = create_train_valid_test_sets(dataset,0.1,0.15,remove=10)
# ytrain=train_valid_test_sets$ytrain
# yvalid=train_valid_test_sets$yvalid
# ytest=train_valid_test_sets$ytest
# 
# # multivariate series with no lags
# dataset=load.dataset("JUMP DIFFUSION")
# train_valid_test_sets = create_train_valid_test_sets(dataset,0.1,0.15)
# 
# ytrain=train_valid_test_sets$ytrain
# yvalid=train_valid_test_sets$yvalid
# ytest=train_valid_test_sets$ytest
# Xtrain=train_valid_test_sets$Xtrain
# Xvalid=train_valid_test_sets$Xtrain
# Xtest=train_valid_test_sets$Xtest

