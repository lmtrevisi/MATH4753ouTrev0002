# Q 14 

library(readxl)
dird = "D:/2020-MATH4753/Laboratories/DataMS6/K25936_Downloads/Excel/"

myread = function(xls){
  require(readxl)
read_xls(paste0(dird, xls))
}

leadcop = myread("LEADCOPP.xls")
solrad = myread("SOLARAD.xls")
diazinon = myread("DIAZINON.xls")

t.test(leadcop$LEAD,conf.level = 0.97)

# Or use a fomula
x = leadcop$LEAD
alpha  =0.03
n = length(x)
mp = c(-1,1)
mean(x) + mp*qt(1-alpha/2,n-1 )*sd(x)/sqrt(n)

names(solrad)
names(diazinon)

with(solrad, t.test(STJOS, IOWA, paired=TRUE, conf.level = 0.80))

with( diazinon, t.test(DAY, NIGHT, paired =TRUE))
