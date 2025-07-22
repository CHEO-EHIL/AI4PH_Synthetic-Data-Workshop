## This is a function for emulating the external COVID-19 risk calculator ##

breastcancer.downstream <- function(train_data, holdout_data){
  
  # defining the predictors and outcome
  predictors <- colnames(train_data)[-length(colnames(train_data))]
  outcome <- "Class"

  # predictive modeling: classification breast cancer versus healthy controls
  library(sdgm)
  sdgm.verbose <- F
  
  # note that the number of iteration is reduced due to time constraints 
  model_lgbm <- sdgm::lgbm.bestmodel.bin(train_data[, c(predictors, outcome)], outcome, n_iter = 1, tune = TRUE, par = TRUE)
  preds <- predict(model_lgbm, holdout_data[, c(predictors, outcome)])
  lgbm_auc <- sdgm::auc(preds,holdout_data[,outcome])
  
  return(lgbm_auc)
}

downstream.summary <- function(data_list, train){
  perform <- lapply(data_list, function (synth){
    augm <- rbind(train, synth)
    perform <- breastcancer.downstream(augm, holdout)
    return(perform)
  })
  perform_avg <- mean(unlist(perform), na.rm = TRUE)
  perform_sd <- sd(unlist(perform), na.rm = TRUE)
  results <- list(perform_avg = perform_avg, perform_sd = perform_sd, perform_list = perform)
  return(results)
}
