ssInflation <- .024
earlyfactor <- function(birthday,retDate){
    # Thsi function takes a person's birthday and retirement date and
    # returns the social security multiplier that determines how much
    # higher or lower the monthly benefit payment will be
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
    lateprice <- min(.08,.03+ceiling((birthyear - 1924)/2)*.005)
    retirementadjustmentmonths <- as.numeric(min(retDate,birthday + 70*365.25)-normalretirementdate)/(365.25/12)
    if (retirementadjustmentmonths < 0){
        retirementadjustmentmonths <- ceiling(retirementadjustmentmonths) 
        if (retirementadjustmentmonths < -36) retfactor <- .8+max(5/1200*(retirementadjustmentmonths-36),-.1)
        if (retirementadjustmentmonths >= -36) retfactor  <- 1+5/900*retirementadjustmentmonths
    }
    if (retirementadjustmentmonths >= 0){
        retirementadjustmentmonths  <-  floor(retirementadjustmentmonths)
        retfactor <- 1+lateprice/12*retirementadjustmentmonths
    }
    return(retfactor)
}

mybirthday <- as.Date('1984-11-18')
today <- Sys.Date()
myage <- floor(as.double(today-mybirthday)/365.25)
now <- as.numeric(format(today,"%Y"))

T2<-97-myage
w0<-25000
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
#p <- exp(res$coefficients[1]  + res$coefficients[2]*time)
#p <- 40711.61 * (1+ssInflation)^(time-2009)
#p[1:length(a$Index)] <- a$Index
#scale <- p/p[1]

avgwage <- 40711.61 
kmax <- 16500
imax <- 5000
kseries <- 500*round(kmax/500*(1+inflation)^t)
#401k contribution limits: http://en.wikipedia.org/wiki/401(k)#Contribution_limits
iseries <- 500*round(imax/500*(1+inflation)^t)
IRAlimit <- iseries
#IRA contribution limits: http://www.guidestoneretirement.org/retirementplans/contributionlimits/futurecontriblimits.aspx
#modelling note: since my default assumption is that tax rates and brackets
#will move with inflation, I think my plan will be to just leave these
#contribution limits equal at a given time, and then implicitly adjust for
#inflation by reinflating a 2008 tax situation to future dollars.

#http://www.ssa.gov/oact/COLA/piaformula.html
initwage <- 9779.44
bp1 <- round(180 * avgwage/initwage)
bp2 <- round(1085 *avgwage/initwage)
estate <- 500000
fedtax <-
    function(X){
        income <- X$income
        over65 <- X$over65
        longtermcapitalgains <- X$longtermcapitalgains
        dividendincome <- X$dividendincome
        dependents <- X$dependents

        married <- 1
        socialsecurityincome <- 0
        state <- 9
        year <- 1997
        spouseincome <- 0
        propertyincome <- 0
        taxablepensions <- 0
        transferincome <- 0 
        rentpaid <- 0
        realestatepaid <- 0
        itemizeddeductions <- 0
        childcareexpense <- 0
        uiincome <- 0
        nonAMTdeductions <- 0
        shorttermcapgains <- 0
        caseid <- 1
#tsfile <-
    #cbind(seq(1,dim(big)[1]),year,state,married,dependents,big$Var2,big$Var1,spouseincome,big$Var4,propertyincome,taxablepensions,socialsecurityincome,transferincome,rentpaid,realestatepaid,itemizeddeductions,childcareexpense,uiincome,big$Var5,nonAMTdeductions,shorttermcapgains,big$Var3)
        tsfile <-
            cbind(seq(1,dim(big)[1]),year,state,married,dependents,over65,income,spouseincome,dividendincome,propertyincome,taxablepensions,socialsecurityincome,transferincome,rentpaid,realestatepaid,itemizeddeductions,childcareexpense,uiincome,dependents,nonAMTdeductions,shorttermcapgains,longtermcapitalgains)
        temp<- file('test.txt')
        temptable<- file('testtable.txt')
        write(headline,temp,append=FALSE) 
        write.table(tsfile, temptable, row.names=FALSE, col.names=FALSE, append=TRUE)
        system('cat testtable.txt >> test.txt')
        ftpUpload('test.txt',url)
        taxsim <- read.table(textConnection(getURL(outputurl)))
        X$fed <- taxsim[4]
        X$state <- taxsim[5]
        #return(taxsim[4])
        return(X)
    }
funevals <- 0
pension <- function(retirementDate,sR,ssType){
    #This function calculates the final yearly permanent income, in real
    #terms, if a person saves at a given rate and gets ss under a given
    #rule, and retires at a certain time.  I will likely in the future make
    #this take income as an input too.

    #Other things that we want to calculate:
    #401k
    #IRA
    #other savings
    #taxable capital gains
    #marginal tax rates
    #To do this, I think i want to make a single call to  taxsim and then
    #interpolate answers.  This isn't going to be quite the best
    #optimization, but should be alright mostly.  I need to vary the
    #following variables to determine taxes for a given individual under a
    #range of possible decisions:
        #income - from min to max
        #capital gains amounts - (here using just the avg return might be
                                 #weird)
        #Kids/marriage/other deducatinos
        # age
        #social security income
        #kid/dependents
        

    # TODO: make a call to fedtax to get tax rates
    # TODO: Integrate taxes into formula

    today <- Sys.Date()
    j <- retirementDate - today
    T1 <- floor(as.double(retirementDate - today)/365.25)
    print(T1)
    print(T2)
    t<-seq(1,T2)
    finances <- data.frame(t)
    returnHistory <- rep(investmentReturn,T2)
    returnHistory[t>=T1] <- retirementInvestmentReturn
    laborincome <- (exp(initialIncome + .1301*t - .0023*t^2)*(1+inflation)^t) * (t < T1)
    print(initialIncome)
    print(inflation)
    #print(finances$laborincome)
    #income comes from Heckman's 50 years of Mincer regressions:
    ##http://time.dufe.edu.cn/mingrendt/lochner030404.pdf, table 2 for white Men in 1990
    #finances$retirementConsumptionPath <- ((1+inflation)^t) * (t >=T1)
    #print(finances$retirementConsumptionPath)
    savingsRate <- sR

    remaining <- function(retirementIncomeGoal){
        print('called remaining')
        print(retirementIncomeGoal)
        #This functino calculates how much money you would be left with if
        #you had given income levels and then spent at retirementIncomeGoal
        #real levels durign retirement
        finances <- calcFinance(retirementIncomeGoal)
        print(finances$savings[T2-1])
        return(-1*sum((finances$savings[T2-1])^2))
    }

    calcFinance <- function(retirementIncomeGoal){
        #This function calculates savings, income, and consumption paths for
        #a given level of retirement consumption.  It also calculates taxes.
        #It returns a 'finances' structure with many elements set.  
        finances$laborincome <- laborincome
        finances$currentSavings <- finances$laborincome * savingsRate
        finances$toIRA <- pmin(finances$currentSavings,IRAlimit)
        finances$taxableincome <- finances$laborincome - finances$toIRA
        finances$deductions <- finances$toIRA 
        finances$retirementConsumptionPath <- ((1+inflation)^t) * (t >=T1)
        print(inflation)
        print(t)
        #TODO: set IRAlimit
        finances$capitalgains <- rep(0,T2)
        finances$capitalgainsrate <- rep(0,T2)
        finances$taxes <-
            taxes(income=finances$laborincome,longtermcapitalgains=finances$capitalgains)
        finances$netincome <- finances$laborincome - finances$taxes$tax + finances$capitalgains
        numiter <- 1
        finances$oldrate <- rep(1,T2)
        while(sum((finances$taxes$rate - finances$capitalgainsrate)^2)>0){
        #while(sum((finances$taxes$rate - finances$capitalgainsrate)^2)>0 && sum((finances$taxes$rate - finances$oldrate)^2) > 0){
            print(funevals)
            finances$oldrate <- finances$capitalgainsrate
            finances$capitalgainsrate <- finances$taxes$rate
            #print(finances$taxes)
            #print(finances$capitalgainsrate)
            X <- diag(T2) -
            rbind(rep(0,T2),cbind(diag(returnHistory[1:T2-1] *
                                       (1-finances$capitalgainsrate[1:T2-1]/100)
                                       + 1),rep(0,T2-1)))
            #rbind(rep(0,T2),cbind(diag(returnHistory[1:T2-1] * (1) + 1),rep(0,T2-1)))
            #Here I attempt to assume that all capital gains are realized as
            #accrued (which is totally weird since there's currently no
            #randomness in returns) but i could change this to be more
            #reasonable 

            q <- savingsRate*finances$laborincome -  finances$retirementConsumptionPath * retirementIncomeGoal 
            q[1] <- q[1]+w0
            print(X[1:5,1])
            finances$savings <- solve(X) %*% q
            print(cbind(finances$savings,finances$returnHistory,finances$capitalgainsrate))
            finances$capitalgains <- finances$savings * finances$returnHistory * finances$capitalgainsrate / 100
            finances$taxes <-
                taxes(income=finances$laborincome,longtermcapitalgains=finances$capitalgains)
            numiter <- numiter + 1
            funevals <- funevals + 1
        }
        finances$ss <- calcSS(ssType,finances)
        finances$consumption <- (1-savingsRate) * finances$laborincome +
            finances$retirementConsumptionPath * retirementIncomeGoal -
            finances$taxes$tax 
        
        return(finances)
    }
    calcSS <- function(type='current',finances){
        #This function calculates social security payments given a lifetime
        #stream of earnings
        if (type == 'none') return(0)
        if (type == 'current'){
            #income <- (exp(initialIncome + .1301*t - .0023*t^2)) 
            ficamax <- 106800*(1+ssInflation)^(t-1)
            indexedIncome <- sapply(finances$laborincome,min,106800*(1+ssInflation)^(t-1))/(avgwage*(1+ssInflation)^(t-1))
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
            indexedIncome <- sapply(finances$laborincome,min,106800*(1+ssInflation)^(t-1))/(avgwage*(1+ssInflation)^(t-1))
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
            indexedIncome <- sapply(finances$laborincome,min,106800*(1+ssInflation)^(t-1))/(avgwage*(1+ssInflation)^(t-1))
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
    optimal <- optimize(remaining, interval=c(10000,1000000))$minimum
    print(optimal)
    fin <- calcFinance(optimal)
    print('finished optimizing')
    print(fin$savings)
    #return(list(consumption=optimal,savings=fin$savings,income=fin$laborincome))
    return(fin)
}

s <- seq(.02,.26,by=.08)
retirementage <- seq(Sys.Date()+29*365.25 ,Sys.Date()+49*365.25, by=5*365.25)

testoutput <- pension(as.Date("2053-10-22"),.15,'current')
z <- matrix(0,nrow=length(s),ncol=length(retirementage),dimnames=c(list(s),list(retirementage)))
noss <- matrix(0,nrow=length(s),ncol=length(retirementage),dimnames=c(list(s),list(retirementage)))
today <- Sys.Date()

for(i in 1:length(s)){
    for(j in 1:length(retirementage)){
        result <- pension(retirementage[j],s[i],'current')
        ssStartTime <- floor(as.double(retirementage[j]-today)/365.25)
        #print(c(result$savings$socialSecurity[result$savings$socialSecurity>0][1]*(1+inflation)^(-ssStartTime),result$consumption))
        z[i,j] <- result$consumption + result$savings$socialSecurity[result$savings$socialSecurity>0][1]*(1+inflation)^(-ssStartTime)
        noss[i,j] <- result$consumption 
        #z and noss are matrices of final permanent incomes at retirement,
        #in current dollars, for retirement at age j and saving at rate s
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

pdf('income.pdf')
plot(t+myage,testoutput$income,type="n",xlab="Age",ylab="Price, in 2011 Dollars")
lines(t+myage,testoutput$income, lty=1)
prices <- exp(initialIncome)*(1+inflation)^t
lines(t+myage,prices, lty=2)
title("Income and Price Levels over Lifetime",sub="Note: Income based on white male age-income distribution from 1990 Census from http://time.dufe.edu.cn/mingrendt/lochner030404.pdf.
Inflation is the historical average of 2% since 1871 from Shiller's calculations http://www.econ.yale.edu/~shiller/data.htm.", cex.sub=.55)
legend(myage,range(testoutput$income)[2],c("Income","Price Levels"),lty=1:2)
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
