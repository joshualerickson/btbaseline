
#libraries
library(caret)
library(CAST)
library(randomForest)
library(tidyverse)
library(doParallel)
library(parallel)
source('utils.R')

#read in data generated from pre-processing

ffs_results_gbm <- data.frame()

for (i in seq_along(index_list)){

  predictorsRF <- train[,c("twi30agg","tpi30agg","accum30","vv30agg", "vvsd30agg","npol30agg","NDVI_med", "nppmmid30agg","deficitRS","cad30RS","decid30RS","cpg30precip","Green_med","NIR_med")]

  responseRF <- train$stream


  #bring in cores
  cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
  registerDoParallel(cluster)


  #set up control
  ctrl <- trainControl(method="repeatedcv",
                       repeats = 5,number = 10,
                       allowParallel = TRUE,
                       returnResamp = "all",
                       verbose = FALSE,
                       classProbs = TRUE,
                       summaryFunction = fourStats,
                       index = index_list[[i]][[1]],
                       savePredictions = 'all')
  #run
  set.seed(1234)
  ffshw <- ffs(predictorsRF,responseRF,
               metric = "Accuracy",
               method = "gbm",
               trControl = ctrl)

  vars <- ffshw$selectedvars

  metrics <- ffshw$results

  metrics <- metrics %>% mutate(vars = list(vars), cluster = paste(names(index_list[i])),
                                model = "gbm")


  ffs_results_gbm <- plyr::rbind.fill(ffs_results_gbm, metrics)
  #now remove cluster. consistency...
  stopCluster(cluster)
  registerDoSEQ()


}

library(dplyr)

#this removes `vars` as list column and makes character
ffs_results <- ffs_results %>%
  mutate(char_vars = str_remove_all(vars, c("^c" = "", "\\(|\\)" = "", "[\"]" = ""))) %>%
  select(-vars)

write_csv(ffs_results, 'all_results_final.csv')
