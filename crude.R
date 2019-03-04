fitted_values <- NULL

for(i in 252:nrow(model_data)){
  
  train <- model_data %>% dplyr::slice(1:i)
  y_fit <- train["tradesignal"]
  train <- train%>%select(-tradesignal)
  train_xgb <- xgb.DMatrix(data =  as.matrix(train[,-1]), label = y_fit$tradesignal)
  
  tmw <- i+1
  
  if(tmw>nrow(model_data)){
    break
  }
  
  tomorrow <- model_data %>% dplyr::slice(tmw) %>% select(-tradesignal)
  tomorrow_xgb <- xgb.DMatrix(data =  as.matrix(tomorrow[,-1]))
  
  cv.nround <- 1000
  cv.nfold  <- 5
  
  mdcv <- xgb.cv(data=train_xgb, nfold=cv.nfold, nrounds=cv.nround, verbose = T, early_stopping_rounds = 50)
  min_logloss <-  mdcv$evaluation_log %>% filter(test_rmse_mean==min(test_rmse_mean))
  xgboostmodel <-  xgboost(data = train_xgb, nrounds = min_logloss$iter)
  
  fitted_values <- 
    predict(xgboostmodel, newdata = tomorrow_xgb) %>% 
    as.tibble() %>% 
    mutate(date = tomorrow$date) %>% 
    rbind(fitted_values)
  
}
