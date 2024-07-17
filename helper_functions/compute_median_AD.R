######## Compute median absolute deviation  from the middle category  ##########

median_AD <- function(x, mid = NULL){ # input is categorical 
  tab <- table(x)
  
  # specify midpoint 
  if(!is.null(mid)){
    midpoint <- mid
  } else if(length(tab) %% 2 != 0){ # num. of categories uneven 
    midpoint <- as.numeric(names(tab[round((length(tab)/2))]))
  } else{ # num. of categories even 
    midpoint <- (as.numeric(names(tab[round((length(tab)/2))])) + 
                   (as.numeric(names(tab[round((length(tab)/2))]))+1)) / 2
  }
  
  # compute absolute deviation from midpoint for each observation
  abs_dev <- c()
  for(obs in seq_along(x)){
    abs_dev[obs] <- abs(x[obs] - midpoint)
  }
  
  median_abs_dev <- median(abs_dev)
  
  return(list("middle category" = midpoint,
              "median absolute deviation from midpoint" = median_abs_dev))
}


# for continuous input 
median_AD_continuous <- function(x, mid){
  abs_dev <- c()
  for(obs in seq_along(x)){
    abs_dev[obs] <- abs(x[obs] - mid)
  }
  
  median_abs_dev <- median(abs_dev)
  
  return(median_abs_dev)
}
