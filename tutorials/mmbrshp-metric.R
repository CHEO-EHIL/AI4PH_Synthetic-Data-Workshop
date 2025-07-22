## This is a function to measure membership disclosure ##

library(arules)
library(dplyr)

calc.mmbrshp <- function(syn_data, training_data, holdout_data, population_size, attack_size, quasi_vars){
  
  # attack dataset: t (member prevalence) must be equal to sampling fraction
  t <- nrow(training_data)/population_size
  members_size <- round(attack_size*t)
  
  # repeat sampling 
  f1_lst <- replicate(500, {
    members <- training_data[sample(nrow(training_data), members_size, replace = F), quasi_vars]
    non_members <- holdout_data[sample(nrow(holdout_data), attack_size - members_size, replace =F), quasi_vars]
    members$origin <- "M"
    non_members$origin <- "NM"
    attack_data <- rbind(members, non_members)
  
    # combine attack and synthetic data for discretizing
    syn_data_discr <- syn_data[quasi_vars]
    syn_data_discr$origin <- "S"
    data_combined <- rbind(attack_data, syn_data_discr)
  
    # discretizing datetime and numerical variables: in case of < 20 rounding the values and take them as category
    datetime_vars <- colnames(attack_data)[sapply(attack_data, function(x) inherits(x, c("POSIXct", "POSIXlt", "Date")))]
    data_combined[datetime_vars] <- lapply(data_combined[datetime_vars], 
                                           function(x) as.numeric(as.Date(x) - as.Date("1900-01-01")))
    num_vars <-  c(colnames(attack_data)[sapply(attack_data, is.numeric)], datetime_vars)
    data_combined[num_vars] <- lapply(num_vars, function(x){
      if (length(unique(data_combined[[x]])) > 20){
        data_combined[[x]] <- arules::discretize(data_combined[[x]], method = "interval", breaks = 20)
      } else {
        data_combined[[x]] <- round(data_combined[[x]])
      }
      data_combined[[x]] <- as.factor(data_combined[[x]])
      return(data_combined[[x]])
    })
  
    # disentangling datasets
    attack_data_discr <- data_combined[data_combined$origin == "M" | data_combined$origin == "NM", ]
    syn_data_discr <- data_combined[data_combined$origin == "S", ]
    
    # exact matching (i.e., Hamming Distance = 0): treating NA as category
    matched_targets <- attack_data_discr %>%
      semi_join(syn_data_discr, by = quasi_vars)
    
    # find non-matches
    non_matched_targets <- attack_data_discr %>%
      anti_join(syn_data_discr, by = quasi_vars)
  
    # define classification measurements (TN not relevant for mmbrshp disclosure)
    tp <- sum(matched_targets$origin == "M")
    fp <- sum(matched_targets$origin == "NM")
    fn <- sum(non_matched_targets$origin == "M")
    precision <- ifelse(tp+fp == 0, 0, tp/(tp+fp))
    recall <- ifelse(tp+fn == 0, 0, tp/(tp+fn))
    f1 <- ifelse(precision+recall == 0, 0, 2*(precision*recall)/(precision+recall))
    
    # output f1
    return(f1 = f1)
  })
  
  # average f1
  f1 <- mean(f1_lst, na.rm = T)
  
  # calculate incremental mmbrshp disclosure as kappa like metric with F_naive as measurement by chance
  f_naive <- 2*t/(1+t)
  f_rel <- (f1-f_naive)/(1-f_naive)
  
  # output f_rel
  return(f_rel = f_rel)
}