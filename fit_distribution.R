library("fitdistrplus")
library("mvtnorm")
library("ggplot2")
library("tidyr")
library("dplyr")


# returns true if a number is an integer
is_wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}

# convert a character column (categorical variable) in a dataframe to an integer column
chars_to_ints <- function(df){
  for(i in 1:ncol(df)) {
    if (typeof(df[,i]) == "character"){ 
      df[,i] <- as.integer(as.factor(df[,i]))
    }
  }
  return(df)
}

# given an input dataframe, 
# return an output dataframe with a synthetic distribution
fit_distribution <- function(df, stat, n_rows, debug_mode = FALSE){
  discrete_pos_dist <- c("binom","nbinom","pois")
  continuous_0_1_dist <- c("beta")
  num_greater_0_dist <- c("gamma","lnorm","weibull")
  num_all <- c("norm","unif")
  
  # for each variable get the best distribution
  best_dist_list <- list()
  for(i in 1:ncol(df)) {
    # ID if a variable is categorical
    is_categorical <- FALSE
    if (typeof(df[,i]) == "character"){ 
      is_categorical <- TRUE
    }
    else{
      # ID if a variable is an integer 
      is_integer <- FALSE
      if (all(is_wholenumber(df[,i]))){
        is_integer <- TRUE
      }
      # ID if a variable is greater than 0
      is_greater_0 <- FALSE
      if (all(df[,i] > 0)){
        is_greater_0 <- TRUE
      }
      # ID if a variable is continuous and between 0 and 1
      is_cts_0_1 <- FALSE
      if (is_integer == FALSE & all(df[,i] >= 0) & all(df[,i] <= 1)){
        is_cts_0_1 <- TRUE
      }
      
      # build the list of allowed distributions for a given variable
      test_dist <- num_all
      if(is_integer == TRUE & is_greater_0 == TRUE){
        test_dist <- append(test_dist, discrete_pos_dist)
      }
      if(is_cts_0_1 == TRUE){
        test_dist <- append(test_dist, continuous_0_1_dist)
      }
      if(is_greater_0 == TRUE){
        test_dist <- append(test_dist, num_greater_0_dist)
      }
    }
    
    if(is_categorical){
      best_dist_list[[length(best_dist_list)+1]] <- list(variable=colnames(df)[i], 
                                                         dist="categorical",
                                                         frequencies=prop.table(table(df[,i]))) 
    }
    else{
      # fit all appropriate distributions
      fits <- data.frame(ind=integer(), dist=character(), value=numeric())
      res_list <- list()
      index <- 0
      for(j in test_dist){ 
        index <- index+1
        if(j=="binom"){
          binom_size = max(df[, i]) # estimate the total number of trials
          res <- fitdist(df[, i], j, 
                         fix.arg=list(size=binom_size), start=list(prob=0.5))
        }
        else{
          res <- fitdist(df[, i], j)
        }
        sum_res <- summary(res)
        if(j=="binom"){
          sum_res$size <- binom_size
        }
        res_list <- append(res_list, list(sum_res))
        fits <- rbind(fits,data.frame(ind=index, dist=j, value=unlist(sum_res[stat])))
      }
      if (stat=="loglik"){ # get the largest loglikelihood (smallest AIC and BIC)
        fits$value <- -(fits$value)
      }
      # save the best fit distribution parameters
      best_dist <- fits[which.min(fits$value),]
      best_dist_params <- list(res_list[[best_dist$ind]]$estimate)
      if (best_dist$dist == "binom"){
        best_dist_params <- append(best_dist_params, list(size=res_list[[best_dist$ind]]$size)) 
      }
      best_dist_list[[length(best_dist_list)+1]] <- list(variable=colnames(df)[i], 
                                                         dist=best_dist$dist,
                                                         is_int=is_integer,
                                                         params=best_dist_params)
    }
  }
  
  if(debug_mode == TRUE){
    cat(file=stderr(), paste0(best_dist_list, collapse = "\n"))
  }
  
  # create multivariate correlated random normal
  corr_df <- chars_to_ints(df)
  correlation <- cor(corr_df)
  multivariate_normal <- rmvnorm(n_rows, sigma = correlation)
  
  for(i in 1:ncol(multivariate_normal)) {       
    uniform_dist <- pnorm(multivariate_normal[,i]) # cdf transform normal to uniform
    # inverse cdf transform uniform to arbitrary dist
    if(best_dist_list[[i]]$dist == "categorical"){ 
      synthetic_var <- character(n_rows)
      tab <- unlist(best_dist_list[[i]]$frequencies)
      names <- names(tab)
      cum_prob=0
      for(j in 1:length(tab)){
        if(j==1){
          synthetic_var[uniform_dist < tab[j]] <- names[j]
        }
        else if (j==length(tab)){
          synthetic_var[uniform_dist >= cum_prob] <- names[j]
        }
        else{
          synthetic_var[uniform_dist >= cum_prob & uniform_dist < (cum_prob+tab[j])] <- names[j]
        }
        cum_prob <- cum_prob + tab[j]
      }
    }else{
      synthetic_var <- do.call(paste0("q",best_dist_list[[i]]$dist),
                               append(list(p=uniform_dist), unlist(best_dist_list[[i]]$params)))
      # round integer variables
      if(best_dist_list[[i]]$is_int == TRUE){
        synthetic_var <- round(synthetic_var, 0) 
      }
    }
    if(i==1){
      synthetic_data <- data.frame(synthetic_var)
    }
    else{
      synthetic_data <- cbind(synthetic_data, synthetic_var)
    }
  }
  colnames(synthetic_data) <- colnames(df)
  return(synthetic_data)
}