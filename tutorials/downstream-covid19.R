## This is a function for emulating the external COVID-19 risk calculator ##

covid19.downstream <- function(train_data, holdout_data){
  library(dplyr)
  
  # defining the outcome: mean as threshold for balanced outcome 
  train_data <- train_data %>% tidyr::drop_na(all_of("risk_infection"))
  train_data$status <- ifelse(train_data$risk_infection >= 12.56, 1, 0)
  holdout_data <- holdout_data %>% tidyr::drop_na(all_of("risk_infection"))
  holdout_data$status <- ifelse(holdout_data$risk_infection >= 12.56, 1, 0)
  outcome <- "status"
  
  # defining the predictors
  predictors <- c("age", "sex", "race", "smoking", 
                  "bmi", "house_count", "public_transport_count", "nursing_home", 
                  "covid19_symptoms", "covid19_contact", "health_worker", "asthma", "kidney_disease", 
                  "liver_disease", "heart_disease", "lung_disease", "diabetes", "hypertension")
  
  
  # predictive modeling: classification of risk records via lgbm
  library(sdgm)
  sdgm.verbose <- F
  
  # note that the number of iteration is reduced due to time constraints 
  model_lgbm <- sdgm::lgbm.bestmodel.bin(train_data[, c(predictors, outcome)], outcome, n_iter = 1, tune = TRUE, par = TRUE)
  preds <- predict(model_lgbm, holdout_data[, c(predictors, outcome)])
  lgbm_auc <- sdgm::auc(preds,holdout_data[,outcome])
  
  return(lgbm_auc)
}
