# cleans environment
rm(list = ls())

# import libraries
library(rlang)
library(tidyverse)
library(quantreg) 
library(qpcR)
library(tseries)
library(NNKCDE)
library(rugarch)
library(FlexCoDE)
library(forecast)

# import methods and auxiliary code
source('./auxiliary/load_datasets.R')
source('./auxiliary/preprocess.R')
source('./auxiliary/utils.R')
source('./method_estimate/QAR.R')
source('./method_estimate/NNKCDE.R')
source('./method_estimate/GARCH.R')
source('./method_estimate/FLEXCODE.R')

# import main functions
source("./test_methods.R")
source("./loss_simulator.R")

# set the main functions parameters
source("./set_params.R")