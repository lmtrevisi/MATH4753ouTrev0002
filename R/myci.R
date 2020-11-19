#' Mean confidence interval
#'
#' @param x Sample
#' @param conf.int interval of confidence
#'
#' @return
#' @export
#'
#' @examples myci(x=rnorm(7,mean=3,sd=1),conf.int=0.95)
myci=function(x = 0, conf.int=0.95 ){
  a= 1-conf.int
  ci=c()
  ci[1]=mean(x)-qt(1-a/2,length(x)-1)*sd(x)/sqrt(length(x))
  ci[2]=mean(x)+qt(1-a/2,length(x)-1)*sd(x)/sqrt(length(x))
  ci
}
