curve(dnorm(x, mean=0,sd=1),xlim=c(-10,30))
# x values corresponding to the x - cords of points on the curve
xcurve=seq(2,30,length=1000)

# Y values corresponding t0 the x values
ycurve=dnorm(xcurve,mean=0,sd=1)

# Fill in the polygon with the given vertices
polygon(c(2,xcurve,30),c(0,ycurve,0),col="Red")
# Area
prob=pnorm(30,mean=0,sd=1)-pnorm(2,mean=0,sd=1)
prob=round(prob,4)

# Click to paste the text onto the graph
text(locator(1), paste("Area = ", prob, sep=""))
