# -------- Function: MM Detection via GMM + roots ----------------
# taken from https://github.com/jmbh/ModalitySkewnessPaper/blob/main/aux_Functions.R


# computes density and roots of its derivative (= number of modes)
DensMMdet <- function(X, adjust=3, noise=0.4, n = 100, plot=FALSE) {
  
  # Add noise
  Xn <- X + rnorm(length(X), 0, noise)
  
  # Density Estimation
  den <- density(Xn, bw="SJ", adjust=adjust, n=n)
  if(plot) plot(den)
  
  # Compute number of reversals
  ni <- length(den$y) 
  
  diff <- den$y[-ni] - den$y[-1]
  
  sign_reversals <- sign(diff[-(ni-1)]) != sign(diff[-1])
  Nrev <- sum(sign_reversals)
  
  Modality <- ceiling(Nrev/2) # since between each mode there is another reversal
  
  outlist <- list("M" = Modality,
                  "den_x" = den$x,
                  "den_y" = den$y)
  
  return(outlist)
  
}