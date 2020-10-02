#' Title:myncurve:
#'
#' @param mu: mean
#' @param sigma: standard deviation
#' @param a: upper limit (tail)
#'
#' @return
#' @export
#'
#' @examples
myncurve = function(mu, sigma,a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  # x values corresponding to the x - cords of points on the curve
  xcurve=seq(mu-3*sigma,a,length=1000)

  # Y values corresponding t0 the x values
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)

  # Fill in the polygon with the given vertices
  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="Red")
  # Area
  prob=pnorm(a,shape1=6,shape2=3)
  prob=round(prob,4)

  # Click to paste the text onto the graph
  text(0.25,1.5, paste("Area = ", prob, sep=""))
}
