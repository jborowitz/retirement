load('taxsim.RData')
#Define parameters
parameters <- NULL
parameters$ssInflation <- .024
parameters$birthday <- as.Date('1984-11-18')
parameters$today <- Sys.Date()
parameters$myage <- floor(as.double(parameters$today-parameters$birthday)/365.25)
parameters$now <- as.numeric(format(parameters$today,"%Y"))
parameters$T2<-97-parameters$myage
parameters$w0<-25000
parameters$t<-seq(1,parameters$T2)
parameters$investmentReturn<-.04
#investmentReturn<-.04 #This return level is from Shiller's website:
#http://www.econ.yale.edu/~shiller/data.htm
parameters$retirementInvestmentReturn<-.03
parameters$inflation<-.02
#initialIncome=log(50000)
parameters$initialIncome=log(90000)
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


#Run and calculate things
s <- seq(.02,.26,by=.08)
retirementage <- seq(Sys.Date()+29*365.25 ,Sys.Date()+49*365.25, by=5*365.25)

z <- matrix(0,nrow=length(s),ncol=length(retirementage),dimnames=c(list(s),list(retirementage)))
noss <- matrix(0,nrow=length(s),ncol=length(retirementage),dimnames=c(list(s),list(retirementage)))
today <- Sys.Date()
allresults <- NULL
for(i in 1:length(s)){
    for(j in 1:length(retirementage)){
        decisions <- NULL
        decisions$retirementDate <- retirementage[j]
        decisions$savingsRate <- s[i]
        decisions$estate <- 10000
        decisions$T1 <- floor(as.double(decisions$retirementDate - parameters$today)/365.25) 
        decisions$returnHistory <- rep(parameters$investmentReturn,parameters$T2)
        decisions$returnHistory[parameters$t>=decisions$T1] <- parameters$retirementInvestmentReturn
        result <- pension(decisions=decisions, parameters=parameters)
        allresults <- append(allresults,result)
        print(result$retirementIncome)
        print(result$socialsecurity[decisions$T1])
        #result <- pension(retirementage[j],s[i],'current')
        #ssStartTime <- floor(as.double(retirementage[j]-today)/365.25)
        #print(c(result$savings$socialSecurity[result$savings$socialSecurity>0][1]*(1+inflation)^(-ssStartTime),result$consumption))
        z[i,j] <- result$retirementIncome
        # + result$savings$socialSecurity[result$savings$socialSecurity>0][1]*(1+inflation)^(-ssStartTime)
        noss[i,j] <- result$retirementIncome - result$socialsecurity[decisions$T1]
        #z and noss are matrices of final permanent incomes at retirement,
        #in current dollars, for retirement at age j and saving at rate s
    }
}
birthday <- parameters$birthday
inflation <- parameters$inflation
t <- parameters$t
retirementInvestmentReturn <- parameters$retirementInvestmentReturn
myage <- parameters$myage
pdf('Pensions.pdf')
a <- floor(as.double(retirementage-parameters$birthday)/365.25)
plot(a,z[length(s),],type="n",ylim=range(z,noss),xlab="Retirement Age",ylab="Retirement Pension Level, in 2011 $")
title(main="Retirement Pension Levels", sub=paste("Note: Income based on white male age-income distribution from 1990 Census from http://time.dufe.edu.cn/mingrendt/lochner030404.pdf.
Stock returns and inflation based on Shiller's average S&P returns and inflation since 1871: http://www.econ.yale.edu/~shiller/data.htm."), cex.sub=.6)
mtext(paste("Initial income:" ,exp(parameters$initialIncome), ",
            Inflation:", parameters$inflation, ", Saving Investment
Return:",parameters$investmentReturn, ", Retirement Return:",
parameters$retirementInvestmentReturn ," Live Until:",parameters$T2+
parameters$myage,sep=""), cex=.6) 
for(i in 1:length(s)){
    lines(a,z[i,],lty=i)
    lines(a,noss[i,],lty=i,col="blue")
}
legend(a[1],range(z)[2],paste("Savings rate:",s),lty=1:length(s))
dev.off()

pdf('income.pdf')
plot(t+myage,testoutput$laborincome,type="n",xlab="Age",ylab="Price, in 2011 Dollars")
lines(t[t<testoutput$decisions$T1]+myage,testoutput$laborincome[t<testoutput$decisions$T1], lty=1)
prices <- exp(parameters$initialIncome)*(1+inflation)^t
lines(t+myage,prices, lty=2)
lines(t[t>=testoutput$decisions$T1]+myage,testoutput$socialsecurity[t>=testoutput$decisions$T1], lty=3)
lines(t+myage,testoutput$consumption, lty=4)
title("Income and Price Levels over Lifetime",sub="Note: Income based on white male age-income distribution from 1990 Census from http://time.dufe.edu.cn/mingrendt/lochner030404.pdf.  
Inflation is the historical average of 2% since 1871 from Shiller's calculations http://www.econ.yale.edu/~shiller/data.htm.", cex.sub=.55)
legend(myage,range(testoutput$laborincome)[2],c("Income","Price Levels","Social Security","Consumption"),lty=1:4)
dev.off()

#My life expectancy stuff is this:
#Life Expectancy: 86.09
#Lower Quartile : 78.48
#Median Lifetime: 88.47
#Upper Quartile : 96.51
#This is from here:http://gosset.wharton.upenn.edu/mortality/perl/CalcForm.html
#I wanted to use a weibull distributino and roughly match the shape of the
#lines in this table: http://www.ssa.gov/oact/NOTES/as120/LifeTables_Body.html
#It looks like pweibull(x,7.75,scale=92.5) looks pretty decent and more or less
#matches my moments

