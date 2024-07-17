######### Adjustment of density based mode detection method     ###########
#                 (originally proposed by Haslbeck et al.)               # 
###########################################################################

# function 
DensMMdet_adaptive_noise <- function(X, adjust=3, n = 256, plot=FALSE) {
  p_value=0
  noise=.01
  while(p_value<.1)
  {
    noise <- noise + .1
    Xn <- X + rnorm(length(X), 0, noise)
    den <- density(Xn, bw="SJ", adjust=adjust, n=n)
    # idea is that NO effect of discrete bars should be detectable
    # this measured by abs(h$mids - round(h$mids))
    # increase noise until this is the case
    k = 5 # factor to control breaks
    h=hist(Xn, breaks = length(Xn)/k, plot=F)
    while(var(abs(h$mids - round(h$mids))) == 0 & k > 0){
      k = k - 1
      h=hist(Xn, breaks = length(Xn)/k, plot=F)
    }
    summ=summary(lm(h$counts ~ abs(h$mids - round(h$mids))))
    if(nrow(summ$coefficients)>1) p_value=summ$coefficients[2,4] else p_value=.1  # if not enough data
  } 
  ni <- length(den$y) 
  diff <- den$y[-ni] - den$y[-1]
  sign_reversals <- sign(diff[-(ni-1)]) != sign(diff[-1])
  Nrev <- sum(sign_reversals)
  Modality <- ceiling(Nrev/2) # since between each mode there is another reversal
  outlist <- list("M" = Modality,
                  "den_x" = den$x,
                  "den_y" = den$y,
                  "noise" = noise)
  
  return(outlist)
}

