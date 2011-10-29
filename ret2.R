#library('quantmod')
#getSymbols('CWUR0000SA0',src='FRED')
#sspayment <- function(income,startyear){
    #avgwage <- 40711.61 
    ##http://www.ssa.gov/oact/COLA/piaformula.html
    #initwage <- 9779.44
    #bp1 <- round(180 * avgwage/initwage)
    #bp2 <- round(1085 *avgwage/initwage)
#}
#ssInflation <- mean(annualReturn(to.yearly(CWUR0000SA0)))
ssInflation <- .024


#goal<-1000000
#goal<-c(1e6,5e6,10e6,15e6)
#requiredSavings<-(goal-w0*(1+investmentReturn)^(T1+1))/(initialIncome*sum((1+investmentReturn)^(T-t))) 
#initialIncome=log(90000)
#income <- (exp(initialIncome + .1301*t - .0023*t^2)) * (t < T1)
##income[t>T1] <- 0

##This code looks to see what rate of savings every year is necessary to meet
##the annual earnings from the retirement income goal of a certain amount of
##money in real terms as an annuity for T2-T1.  I.e. one could receive an
##annuity of $50,000 per year from retirement for 30 years with the savings
##rate from the first value.
##Timing is: in the first period you start with 
##savings[1] <- w0*(1+investmentReturn)
##for (i in 2:T1+1) {
    ##savings[i] <- savings[i-1]*(1+retirementInvestmentReturn)+income[i]*savingsRate
    ##print(savings[i])
##}
earlyfactor <- function(birthday,retDate){
    today <- Sys.Date()
    now <- as.numeric(format(today,"%Y"))
    myage <- floor(as.double(today-birthday)/365.25)
    birthyear <- as.numeric(format(birthday,"%Y"))
    if (birthyear < 1938)  normalretirementdate <- birthday + 65*365.25 
    if (birthyear >= 1938 & birthyear < 1943) normalretirementdate <- birthday + 65*365.25 +365.35 * (birthyear - 1937)/6
    if (birthyear >= 1943 & birthyear < 1955) normalretirementdate <- birthday + 66*365.25
    if (birthyear >= 1955 & birthyear < 1961) normalretirementdate <- birthday + 66*365.25 +365.35 * (birthyear - 1954)/6
    if (birthyear > 1960) normalretirementdate <- birthday + 67*365.25
    as.numeric(format(normalretirementdate,"%Y"))-birthyear

    #if (birthyear < 1938)  earlyprice <- birthday + 65*365.25 
    #if (birthyear >= 1938 & birthyear < 1943) normalretirementdate <- birthday + 65*365.25 +365.35 * (birthyear - 1937)/6
    #if (birthyear >= 1943 & birthyear < 1955) normalretirementdate <- birthday + 66*365.25
    #if (birthyear >= 1955 & birthyear < 1961) normalretirementdate <- birthday + 66*365.25 +365.35 * (birthyear - 1954)/6
    #if (birthyear > 1960) normalretirementdate <- birthday + 67*365.25

    #print(c(normalretirementdate,retDate,min(retDate,birthday+70*365.25)))
    lateprice <- min(.08,.03+ceiling((birthyear - 1924)/2)*.005)
    #print(lateprice)
    #print(as.numeric(min(retDate,birthday + 70*365.25)-normalretirementdate))
    retirementadjustmentmonths <- as.numeric(min(retDate,birthday + 70*365.25)-normalretirementdate)/(365.25/12)
    #print(normalretirementdate)
    #print(retirementadjustmentmonths)
    if (retirementadjustmentmonths < 0){
        retirementadjustmentmonths <- ceiling(retirementadjustmentmonths) 
        if (retirementadjustmentmonths < -36) retfactor <- .8+max(5/1200*(retirementadjustmentmonths-36),-.1)
        if (retirementadjustmentmonths >= -36) retfactor  <- 1+5/900*retirementadjustmentmonths
    }
    if (retirementadjustmentmonths >= 0){
        retirementadjustmentmonths  <-  floor(retirementadjustmentmonths)
        #print(retirementadjustmentmonths)
        retfactor <- 1+lateprice/12*retirementadjustmentmonths
    }
    return(retfactor)
}
    #retirementadjustmentmonths <- floor(retirementadjustmentmonths) if retirementadjustmentmonths > 0
    #print(as.numeric(retirementadjustment))
#}

mybirthday <- as.Date('1984-11-18')
a1<-earlyfactor(mybirthday,as.Date('2052-4-18'))
a2<-earlyfactor(mybirthday,as.Date('2046-4-18'))
a3<-earlyfactor(mybirthday,as.Date('2066-4-18'))
#print(c(a1,a2,a3))
today <- Sys.Date()
myage <- floor(as.double(today-mybirthday)/365.25)
now <- as.numeric(format(today,"%Y"))
#now <- as.numeric(format(today,"%Y"))
#myage <- as.numeric(format(today,"%Y")) - as.numeric(format(mybirthday,"%Y"))

T2<-97-myage
w0<-25000
#w0 <- 0
t<-seq(1,T2)
investmentReturn<-.04
#investmentReturn<-.04
#This return level is from Shiller's website:
#http://www.econ.yale.edu/~shiller/data.htm
retirementInvestmentReturn<-.03
inflation<-.02
#initialIncome=log(50000)
initialIncome=log(90000)
a <- read.csv('wages.csv',head=TRUE,sep="\t")
#These are historical wage levels for the last 59 years for SS
res <- line(a$Year,log(a$Index))
#incomeIndex[a$Year] <- a$Index
time <- seq(min(a$Year), now+T2-1)
p <- exp(res$coefficients[1]  + res$coefficients[2]*time)
p <- 40711.61 * (1+ssInflation)^(time-2009)
p[1:length(a$Index)] <- a$Index
scale <- p/p[1]

avgwage <- 40711.61 
#http://www.ssa.gov/oact/COLA/piaformula.html
initwage <- 9779.44
bp1 <- round(180 * avgwage/initwage)
bp2 <- round(1085 *avgwage/initwage)
estate <- 500000
pension <- function(retirementDate,sR,ssType){
    today <- Sys.Date()
    j <- retirementDate - today
    T1 <- floor(as.double(retirementDate - today)/365.25)
    #mybirthday
    #now <- as.numeric(format(today,"%Y"))
    #myage <- floor(as.double(today-birthday)/365.25)
    #birthyear <- as.numeric(format(birthday,"%Y"))
    returnHistory <- rep(investmentReturn,T2-1)
    returnHistory[t>=T1] <- retirementInvestmentReturn
    income <- (exp(initialIncome + .1301*t - .0023*t^2)*(1+inflation)^t) * (t < T1)
    #income <- (exp(initialIncome + .03*t)*(1+inflation)^t) * (t < T1)
    #income comes from Heckman's 50 years of Mincer regressions:
    ##http://time.dufe.edu.cn/mingrendt/lochner030404.pdf, table 2 for white Men in 1990

    #income <- ts(income,start=now,frequency=1)
    #income <- as.xts(income)
    #income <- scale*(exp(initialIncome + .1301*t - .0023*t^2)) * (t < T1)
    #note: I think income growth just seems really spectacular unless I
    #assume that it's nominal.  But my buest guess is really that the
    #equation from Heckman Lochner Todd must be real, since it's all in a
    #cross-section.  To make it real, just use the version of this income
    # equation with scale
    retirementConsumptionPath <- ((1+inflation)^t) * (t >=T1)
    savingsRate <- sR
    remaining <- function(retirementIncomeGoal){
        X <- diag(T2) - rbind(rep(0,T2),cbind(diag(returnHistory[2:T2]+1),rep(0,T2-1)))
        q <- savingsRate*income -  retirementConsumptionPath * retirementIncomeGoal
        q[1] <- q[1]+w0
        savings <- solve(X) %*% q
        #return((savings[T2]-estate*(1+inflation)^T2)^2)
        return((savings[T2])^2)
    }
    calcSavings <- function(retirementIncomeGoal){
        X <- diag(T2) - rbind(rep(0,T2),cbind(diag(returnHistory[2:T2]+1),rep(0,T2-1)))
        q <- savingsRate*income -  retirementConsumptionPath * retirementIncomeGoal
        q[1] <- q[1]+w0
        savings <- solve(X) %*% q
        #return((savings[T2]-estate*(1+inflation)^T2)^2)
        ss <- calcSS(ssType)
        return(list(savings=savings,consumption=(1-savingsRate)*income + retirementConsumptionPath * retirementIncomeGoal,socialSecurity=ss))
    }
    calcSS <- function(type='current'){
        if (type == 'none') return(0)
        if (type == 'current'){
            #income <- (exp(initialIncome + .1301*t - .0023*t^2)) 
            ficamax <- 106800*(1+ssInflation)^(t-1)
            indexedIncome <- sapply(income,min,106800*(1+ssInflation)^(t-1))/(avgwage*(1+ssInflation)^(t-1))
            aime <- mean(sort(indexedIncome,decreasing=TRUE)[1:35])
            bp1 <- 180 /initwage
            bp2 <- 1085 /initwage
            piaFactor <- .9*min(aime,bp1) + .32*(min(aime,bp2)-bp1) + .15*min(aime-bp2,0)
            yearlyFactor <- 12 * piaFactor
            early <- earlyfactor(mybirthday,retirementDate)
            ss <- early * yearlyFactor * avgwage*(1+ssInflation)^(t-1) * (t >=T1)
            return(ss)
        }
        if (type == 'bowles-simpson'){
            ficamax <- 106800*(1+ssInflation)^(t-1)
            indexedIncome <- sapply(income,min,106800*(1+ssInflation)^(t-1))/(avgwage*(1+ssInflation)^(t-1))
            aime <- mean(sort(indexedIncome,decreasing=TRUE)[1:35])
            bp1 <- 180 /initwage
            bp2 <- 1085 /initwage
            piaFactor <- .9*min(aime,bp1) + .32*(min(aime,bp2)-bp1) + .15*min(aime-bp2,0)
            yearlyFactor <- 12 * piaFactor
            early <- earlyfactor(mybirthday,retirementDate)
            ss <- early * yearlyFactor * avgwage*(1+ssInflation)^(t-1) * (t >=T1)
            return(ss)
        }
        if (type == 'domenici-rivlin'){
            ficamax <- 106800*(1+ssInflation)^(t-1)
            indexedIncome <- sapply(income,min,106800*(1+ssInflation)^(t-1))/(avgwage*(1+ssInflation)^(t-1))
            aime <- mean(sort(indexedIncome,decreasing=TRUE)[1:35])
            bp1 <- 180 /initwage
            bp2 <- 1085 /initwage
            piaFactor <- .9*min(aime,bp1) + .32*(min(aime,bp2)-bp1) + .15*min(aime-bp2,0)
            yearlyFactor <- 12 * piaFactor
            early <- earlyfactor(mybirthday,retirementDate)
            ss <- early * yearlyFactor * avgwage*(1+ssInflation)^(t-1) * (t >=T1)
            return(ss)
        }
    }
    optimal <- optimize(remaining, interval=c(1000,1000000))$minimum
    return(list(consumption=optimal,savings=calcSavings(optimal),income=income))
}

s <- seq(.02,.26,by=.08)
age <- seq(29,54, by=5)
#retirementage <- age + myage
retirementage <- seq(Sys.Date()+29*365.25 ,Sys.Date()+49*365.25, by=5*365.25)
testoutput <- pension(as.Date("2083-10-22"),.15,'current')
#retirementage <- c("2041-11-18","2051-11-18","2061-11-18")
z <- matrix(0,nrow=length(s),ncol=length(retirementage),dimnames=c(list(s),list(retirementage)))
noss <- matrix(0,nrow=length(s),ncol=length(retirementage),dimnames=c(list(s),list(retirementage)))
today <- Sys.Date()

for(i in 1:length(s)){
    for(j in 1:length(retirementage)){
        result <- pension(retirementage[j],s[i],'current')
        #z[i,j] <- result$consumption
        #print(result$savings$socialSecurity)
        ssStartTime <- floor(as.double(retirementage[j]-today)/365.25)
        #firstSSPayment <- result$savings$socialSecurity[result$savings$socialSecurity>0][1]*(1+inflation)^(-ssStartTime)
        print(c(result$savings$socialSecurity[result$savings$socialSecurity>0][1]*(1+inflation)^(-ssStartTime),result$consumption))
        z[i,j] <- result$consumption + result$savings$socialSecurity[result$savings$socialSecurity>0][1]*(1+inflation)^(-ssStartTime)
        noss[i,j] <- result$consumption 
    }
}

pdf('Pensions.pdf')
a <- floor(as.double(retirementage-mybirthday)/365.25)
plot(a,z[length(s),],type="n",ylim=range(z,noss),xlab="Retirement Age",ylab="Retirement Pension Level, in 2011 $")
title(main="Retirement Pension Levels", sub=paste("Note: Income based on white male age-income distribution from 1990 Census from http://time.dufe.edu.cn/mingrendt/lochner030404.pdf.
Stock returns and inflation based on Shiller's average S&P returns and inflation since 1871: http://www.econ.yale.edu/~shiller/data.htm."), cex.sub=.6)
mtext(paste("Initial income:" ,exp(initialIncome), ", Inflation:", inflation, ", Saving Investment Return:",investmentReturn, ", Retirement Return:", retirementInvestmentReturn ," Live Until:",T2+myage,sep=""), cex=.6) 
for(i in 1:length(s)){
    lines(a,z[i,],lty=i)
    lines(a,noss[i,],lty=i,col="blue")
}
legend(a[1],range(z)[2],paste("Savings rate:",s),lty=1:length(s))
dev.off()

#income <- (exp(initialIncome + .1301*t - .0023*t^2)) 
#ficamax <- 106800*(1+ssInflation)^(t-1)
#indexedIncome <- sapply(income,min,106800*(1+ssInflation)^(t-1))/(avgwage*(1+ssInflation)^(t-1))
#aime <- mean(sort(indexedIncome,decreasing=TRUE)[1:35])
#bp1 <- 180 /initwage
#bp2 <- 1085 /initwage
#piaFactor <- .9*min(aime,bp1) + .32*(min(aime,bp2)-bp1) + .15*min(aime-bp2,0)
#yearlyFactor <- 12 * piaFactor
#ss <- yearlyFactor * avgwage*(1+ssInflation)^(t-1) * (t >=T1)

pdf('income.pdf')
plot(t+myage,testoutput$income,type="n",xlab="Age",ylab="Price, in 2011 Dollars")
lines(t+myage,testoutput$income, lty=1)
prices <- exp(initialIncome)*(1+inflation)^t
lines(t+myage,prices, lty=2)
title("Income and Price Levels over Lifetime",sub="Note: Income based on white male age-income distribution from 1990 Census from http://time.dufe.edu.cn/mingrendt/lochner030404.pdf.
Inflation is the historical average of 2% since 1871 from Shiller's calculations http://www.econ.yale.edu/~shiller/data.htm.", cex.sub=.55)
legend(myage,range(testoutput$income)[2],c("Income","Price Levels"),lty=1:2)
dev.off()

##remaining <- function(sR,t1,t2,rH,inc,retConPath,w0){
##X <- diag(t2) - rbind(rep(0,t2),cbind(diag(rH[2:t2]+1),rep(0,t2-1)))
##q <- sR*inc -  retConPath * retirementIncomeGoal
##q[1] <- q[1]+w0
##savings <- solve(X) %*% q
##return(savings[t2]
##}

#mult<-((1+retirementInvestmentReturn)/(1+inflation))^(-(t-(T1+1)))
#for (i in (T1+1):T2) {
    #savings[i] <-  savings[i-1]*(1+retirementInvestmentReturn[i]) - retirementConsumptionPath[i] * retirementIncomeGoal
#}
#X <- diag(T2) - rbind(rep(0,T2),cbind(diag(retirementInvestmentReturn[2:T2]+1),rep(0,T2-1)))
#q <- savingsRate*income - retirementConsumption

##savings[T1] <- savings[T1-1]*(1+retirementInvestmentReturn)+income[T1]*savingsRate
#retirementIncomeGoal<-c(4000,40e3,75e3,100e3,200e3)
#retirementIncomeGoal<-80e3
#inflatedRetirementIncomeGoal<-retirementIncomeGoal*(1+inflation)^T1
#t<-seq(T2+1,T2)
#mult<-((1+retirementInvestmentReturn)/(1+inflation))^(-(t-(T1+1)))
#retirementNeed<-inflatedRetirementIncomeGoal*sum(mult)
#retiredsavings <- 0*requiredSavings
#retiredsavings[1] <- w0+income[1]*savingsRate
#for (i in (T1+1):T2) {
    #print(c(i,savings[i-1]*(1+inflatedRetirementIncomeGoal),retirementIncomeGoal*(1+inflation)^(i-(T1+1))))
    #savings[i] <-  savings[i-1]*(1+retirementInvestmentReturn) - inflatedRetirementIncomeGoal*(1+inflation)^(i-(T1+1))
#}
#t<-seq(0,T1)
#requiredSavings<-(retirementNeed-w0*(1+investmentReturn)^(T1+1))/(initialIncome*sum((1+investmentReturn)^(T-t)))
##plot(retirementIncomeGoal,requiredSavings)
#output <- cbind(retirementIncomeGoal,inflatedRetirementIncomeGoal,retirementNeed)
#output <- cbind(output,requiredSavings)
#output

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
