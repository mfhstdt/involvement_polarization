##### function to split sample into 4 quartiles based on involvement scores #####

split_involvement <- function(x){
  group <- c()
  
  # compute cut-offs
  quartiles <-  quantile(x, probs = c(0.25, 0.5, 0.75))
  group <- ifelse(x < quartiles[1], 1, 
                                 ifelse(x < quartiles[2], 2, 
                                        ifelse(x < quartiles[3], 3,
                                               4)))
  
  # return vector indicating which quartile observations belong to
  return(group)
}
