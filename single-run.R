load('taxsim.RData')
load('Model.RData')
source('ret2.R')
#Define parameters
parameters <- NULL
parameters$ssInflation <- .024
parameters$birthday <- as.Date('1984-11-18')
parameters$today <- Sys.Date()
parameters$myage <- floor(as.double(parameters$today-parameters$birthday)/365.25)
#parameters$now <- as.numeric(format(parameters$today,"%Y"))
parameters$T2<-97-parameters$myage
parameters$w0<-25000
parameters$t<-seq(1,parameters$T2)
parameters$investmentReturn<-.04
#investmentReturn<-.04 #This return level is from Shiller's website:
#http://www.econ.yale.edu/~shiller/data.htm
parameters$retirementInvestmentReturn<-.03
parameters$inflation<-.02
#initialIncome=log(50000)
parameters$initialIncome=100000
parameters$randomizeIncome <- FALSE
parameters$seed <- 101
#This should be a logarithmic initial income
a <- read.csv('wages.csv',head=TRUE,sep="\t")
#These are historical wage levels for the last 59 years for SS
res <- line(a$Year,log(a$Index))
#incomeIndex[a$Year] <- a$Index
#time <- seq(min(a$Year), now+T2-1)
#p <- exp(res$coefficients[1]  + res$coefficients[2]*time)
#p <- 40711.61 * (1+ssInflation)^(time-2009)
#p[1:length(a$Index)] <- a$Index
#scale <- p/p[1]
parameters$avgwage <- 40711.61 
parameters$race <- "White"
parameters$hispanic <- "Non-Hispanic"
parameters$educ <- 20
kmax <- 16500
imax <- 5000
kseries <- 500*round(kmax/500*(1+parameters$inflation)^parameters$t)
#401k contribution limits: http://en.wikipedia.org/wiki/401(k)#Contribution_limits
iseries <- 500*round(imax/500*(1+parameters$inflation)^parameters$t)
parameters$IRAlimit <- iseries
#IRA contribution limits: http://www.guidestoneretirement.org/retirementplans/contributionlimits/futurecontriblimits.aspx
#modelling note: since my default assumption is that tax rates and brackets
#will move with inflation, I think my plan will be to just leave these
#contribution limits equal at a given time, and then implicitly adjust for
#inflation by reinflating a 2008 tax situation to future dollars.

#http://www.ssa.gov/oact/COLA/piaformula.html
parameters$initwage <- 9779.44
#parameters$bp1 <- round(180 * avgwage/initwage)
#parameters$bp2 <- round(1085 *avgwage/initwage)


parameters$ssType <- 'current'
Rprof()
decisions <- NULL
decisions$retirementDate <- as.Date("2053-10-22")
decisions$savingsRate <- .08
decisions$estate <- 10000
#Estate in current dollars
decisions$T1 <- floor(as.double(decisions$retirementDate - parameters$today)/365.25) 
decisions$returnHistory <- rep(parameters$investmentReturn,parameters$T2)
decisions$returnHistory[parameters$t>=decisions$T1] <- parameters$retirementInvestmentReturn
testoutput <- pension(decisions=decisions, parameters=parameters)
print(testoutput$retirementIncome)
Rprof(NULL)
summaryRprof()
