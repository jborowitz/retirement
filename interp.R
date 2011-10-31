library('RCurl')
load('taxsim.RData')
url<-paste('ftp://taxsim:02138@taxsimftp.nber.org/tmp/',format(Sys.time(),"%Y%m%d%H%M%S"),sep='')
outputurl<-paste(url,'taxsim',sep='.')
headline<-"9 11 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "

fedtax <-
    function(income,over65,longtermcapitalgains,dividendincome,dependents){
        big<-expand.grid(income, over65,longtermcapitalgains,dividendincome,dependents)
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
tsfile <-
    cbind(seq(1,dim(big)[1]),year,state,married,dependents,big$Var2,big$Var1,spouseincome,big$Var4,propertyincome,taxablepensions,socialsecurityincome,transferincome,rentpaid,realestatepaid,itemizeddeductions,childcareexpense,uiincome,big$Var5,nonAMTdeductions,shorttermcapgains,big$Var3)
        #tsfile <-
            #cbind(caseid,year,state,married,dependents,over65,income,spouseincome,dividendincome,propertyincome,taxablepensions,socialsecurityincome,transferincome,rentpaid,realestatepaid,itemizeddeductions,childcareexpense,uiincome,dependents,nonAMTdeductions,shorttermcapgains,longtermcapitalgains)
        temp<- file('test.txt')
        temptable<- file('testtable.txt')
        write(headline,temp,append=FALSE) 
        write.table(tsfile, temptable, row.names=FALSE, col.names=FALSE, append=TRUE)
        system('cat testtable.txt >> test.txt')
        ftpUpload('test.txt',url)
        taxsim <- read.table(textConnection(getURL(outputurl)))
        return(taxsim[4])

    }
q<-fedtax(seq(50000,100000,by=10000),seq(0,1),0,0,1)
system.time(fedtax(seq(50000,100000,by=10000),seq(0,1),0,0,1))
print(a)
