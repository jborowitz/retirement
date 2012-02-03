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
