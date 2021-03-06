% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mychisim.R
\name{mychisim}
\alias{mychisim}
\title{Chi-square statistic}
\usage{
mychisim(
  n1 = 10,
  sigma1 = 3,
  mean1 = 5,
  iter = 1000,
  ymax = 0.1,
  x = 20,
  y = 0.1
)
}
\arguments{
\item{n1}{sample size}

\item{sigma1}{standard deviation of the population}

\item{mean1}{mean of the population}

\item{iter}{number of iteration}

\item{ymax}{parameter to adjust the height of the plot}

\item{x}{x coordinate of legend}

\item{y}{y coordinate of legend}
}
\value{

}
\description{
Chi-square statistic
}
\examples{
mychisim(n1=10,sigma1=3,mean1=5,iter=1000,ymax=0.1,x=20, y=0.1)
}
