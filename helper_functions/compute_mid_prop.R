#### Function to compute proportion of respondents choosing middle category ###
############################## of the scale ####################################

mid_prop <- function(x, mid = NULL){ # input is categorical data, optionally specify middle category
  tab <- table(x)
  
  if(!is.null(mid)){ # when middle category specified
    midpoint = mid
    prop <- (tab[midpoint] / length(x))
    
  } else if((length(tab) %% 2 != 0)){ # if num of categories uneven
    midpoint <- names(tab[round((length(tab)/2))])
    prop <- (tab[midpoint] / length(x))
    
  } else { # if uneven take two middle categories
    midpoint <- c(names(tab[length(tab)/2]),
                  names(tab[(length(tab)/2)+1]))
    prop <- (sum(tab[midpoint]) / length(x))
  }
  
  return(prop)
}


#function for continuous input 
# input is continuous vector x and vector storing beginning and end of scale 
mid_prop_continuous <- function(x, mid = NULL, cats = c(0,10)){ 
  # categorize data 
  breaks <- (cats[1]-0.5) + (cats[1]-1):cats[2]
  h <- hist(x, breaks = breaks, plot = FALSE)
  
  # if specified use prefered midpoint 
  if(!is.null(mid)){
    midpoint <- mid
    index_midpoint <- which(h$mids == midpoint)
    prop <- (h$counts[index_midpoint] / length(x))
    
  # otherwise compute midpoint  
  } else if(length(h$mids) %% 2 != 0){ # uneven number of categories
      midpoint <- round(length(h$mids) / 2)
      index_midpoint <- which(h$mids == midpoint)
      prop <- (h$counts[index_midpoint] / length(x))
      
  } else{
      midpoint <- c((length(h$mids)/2),
                    (length(h$mids)/2 + 1))
      index_midpoint <- match(midpoint, h$mids)
      prop <- (sum(h$counts[index_midpoint]) / length(x))
    }
  return(prop)
}


