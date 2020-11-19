#' F simulation
#'
#' @param n1 Sample Size 1
#' @param n2 Sample Size 2
#' @param sigma1 Standard Deviation of Population 1
#' @param sigma2 Standard Deviation of Population 2
#' @param mean1 mean of population 1
#' @param mean2 mean of population 2
#' @param iter number of iterations
#' @param ymax parameter to adjust the height of the plot
#' @param x x coordinate of legend
#' @param y y coordinate of legend
#' @param ...
#'
#' @return
#' @export
#'
#' @examples myFsim2(n1=10,n2=14,sigma1=3,sigma2=2,mean1=5,mean2=10,iter=1000,ymax=0.9,x=6,y=0.5)
myFsim2=function(n1=10,n2=14,sigma1=3,sigma2=2,mean1=5,mean2=10,iter=1000,ymax=0.9,x=6,y=0.5,...){
  y1=rnorm(n1*iter,mean=mean1,sd=sigma1)# generate iter samples of size n1
  y2=rnorm(n2*iter,mean=mean2,sd=sigma2)
  data1.mat=matrix(y1,nrow=n1,ncol=iter,byrow=TRUE) # Each column is a sample size n1
  data2.mat=matrix(y2,nrow=n2,ncol=iter,byrow=TRUE)
  ssq1=apply(data1.mat,2,var) # ssq1 is s squared
  ssq2=apply(data2.mat,2,var)
  #spsq=((n1-1)*ssq1 + (n2-1)*ssq2)/(n1+n2-2) # pooled s squared
  w=ssq1*sigma2^2/(ssq2*sigma1^2) #
  hist(w,freq=FALSE, ylim=c(0,ymax), # Histogram with annotation
       main=substitute(paste("Sample size = ",n[1]+n[2]," = ",n1+n2," statistic = ",F)),
       xlab=paste("F Statistic",sep=""), las=1)
  lines(density(w),col="Blue",lwd=3) # add a density plot
  curve(df(x,n1-1,n2-1),xlim=c(0,6),add=TRUE,col="Red",lty=2,lwd=3) # add a theoretical curve
  title=expression(F==frac(s[1]^2,s[2]^2)*frac(sigma[2]^2,sigma[1]^2)) #mathematical annotation -see ?plotmath
  legend(x,y,c("Simulated","Theoretical"),col=c("Blue","Red"),lwd=4,lty=1:2,bty="n",title=title)# Legend #
  invisible(list(w=w,summary=summary(w),sd=sd(w),fun="F")) # some output to use if needed
}
