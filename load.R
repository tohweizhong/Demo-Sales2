
# load.R

# packages
library(shinythemes)
library(caret)
library(rpart)
library(rpart.plot)
library(pROC)
library(Metrics)
library(DT)
library(shinydashboard)
library(ggplot2)

# data
load("data/Xtt.RData")
load("data/vars.RData")
load("data/G1.RData")
load("data/G2.RData")
load("data/G3.RData")

# models
load("models/tr0_G1.RData")
load("models/tr0_G2.RData")
load("models/lm0_G3.RData")

# C:/Users/weizhong/Documents/R/Demo-Sales