#P(Y=10), Y~NegBin(p=0.4,r=3).   (Book theory)
#  This means that in R we need n=3 (size), x=y-r=10-3=7
# Nu Failures, Nu of successes, prob success
#' Title
#'
#' @param y Number of trials until the rth success is observed
#' @param r Number of Successes
#' @param p Probability of success on a single Bernoulli trial
#'
#' @return
#' @export
#'
#' @examples
mynbin=function(y,r,p){
  choose(y-1,r-1)*p^r*(1-p)^(y-r)
}

