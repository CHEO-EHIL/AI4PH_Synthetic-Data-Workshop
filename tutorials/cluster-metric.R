## This is a function to measure fidelity by clustering ##

library(arules)
library(ClusterR)

utility.cluster <- function(real_data, syn_data, n_cluster){
  real_data$origin <- "R"
  syn_data$origin <- "S"
  cluster_df <- rbind(real_data, syn_data)
  
  # transform datetime variables
  datetime_vars <- colnames(cluster_df)[sapply(cluster_df, 
                                               function(x) inherits(x, c("POSIXct", "POSIXlt", "Date")))]
  cluster_df[datetime_vars] <- lapply(cluster_df[datetime_vars], 
                                      function(x) as.numeric(as.Date(x) - as.Date("1900-01-01")))
  
  # transform numerical variables
  num_vars <- colnames(cluster_df)[sapply(cluster_df, is.numeric)]
  cluster_df[num_vars] <- lapply(num_vars, function(x){
    if (length(unique(cluster_df[[x]])) > 20){
      cluster_df[[x]] <- arules::discretize(cluster_df[[x]], method = "interval", breaks = 20)
    } else {
      cluster_df[[x]] <- round(cluster_df[[x]])
    }
    cluster_df[[x]] <- as.character(cluster_df[[x]])
    return(cluster_df[[x]])
  })
  cluster_df <- as.data.frame(lapply(cluster_df, addNA))
  cluster_df[] <- as.data.frame(lapply(cluster_df, as.integer))
  
  # clustering
  cluster_clara <- ClusterR::Clara_Medoids(cluster_df[, -ncol(cluster_df)], clusters = n_cluster, 
                                           samples = 5, sample_size = 0.5, distance_metric = "hamming", threads = 5)
  cluster_member <- table(cluster_clara$clusters, cluster_df$origin)
  U <- (4/n_cluster)*sum(((cluster_member[,1]/(cluster_member[,1] + cluster_member[,2]))- 0.5)^2)
  
  # transform U to maximize
  U_max <- 1-U
  
  return(U_max=U_max)
}