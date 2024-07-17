# function to print means and SD per involvement level
print_descriptives <- function(df, variable_inv, index_attitude){
  means <- c()
  SDs <- c()
  print <- c()
  for(i in 1:length(unique(variable_inv))){
    means[i] <- mean(df[variable_inv == names(table(variable_inv))[i], index_attitude])
    SDs[i] <- sd(df[variable_inv == names(table(variable_inv))[i], index_attitude])
    print <- c(print, paste("Involvement", i, "mean", means[i], "sd", SDs[i], sep = " "))
  }
  return(print)
}