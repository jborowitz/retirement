#This file estimates a simple log earnings (and income) model as in Gottschalk
#and Moffitt 2011
#(http://www.econ2.jhu.edu/people/Moffitt/moffitt_gottschalk_2011-2-12.pdf),
#which predicts life-cycle income for the average person in a race-education
#cell.  An individuals income process is simulated relative to this expectation,
#again as in Gottschalk and Moffitt

#In order to use this model, do "predict" with either the fullearnings or
#fullincome model and the "me" data frame, which has race, education, and age
#info.  It depends on a Stata data format CPS March (ASEC) data set from 2011,
#which is created with the NBER's file formats

library('foreign')
rm(list=ls())
a<-read.dta('~/cpsmarch/cpsmar2011.dta',convert.factors=FALSE)
a <- subset(a,wewkrs==1 & pearnval > 0 & ptotval > 0 & a_age <= 85)
#facts<-read.dta('~/cpsmarch/cpsmar2011.dta')
attach(a)
educ <- a_hga
hispanic <- factor(pehspnon,labels=c("Hispanic","Non-Hispanic"))
race <- prdtrace
race[race>2] <- 3
race <- factor(race,labels=c("White","Black","Other"))
educ <-a_hga
educ[a_hga == 31] <- 1
educ[a_hga == 32] <- 2.5
educ[a_hga == 33] <- 5.5
educ[a_hga == 34] <- 7.5
educ[a_hga == 35] <- 9
educ[a_hga == 36] <- 10
educ[a_hga %in% c(37,38)] <- 11
educ[a_hga == 39] <- 12
educ[a_hga == 40] <- 13
educ[a_hga %in% c(41,42)] <- 14
educ[a_hga == 43] <- 16
educ[a_hga == 44] <- 18
educ[a_hga >  44] <- 20
age2 <- a_age^2
age3 <- a_age^3
age4 <- a_age^4
age5 <- a_age^5
age6 <- a_age^6
ly <- log(pearnval)
lt <- log(ptotval)
fullearnings <- lm(ly ~ educ:race:a_age:hispanic + educ:race:age2:hispanic +
                   educ:race:age3:hispanic + educ:race:age4:hispanic +
                   educ:race:age5:hispanic + educ:race:age6:hispanic,
                   weight=marsupwt)
fullincome <- lm(lt ~ educ:race:a_age:hispanic + educ:race:age2:hispanic +
                 educ:race:age3:hispanic + educ:race:age4:hispanic +
                 educ:race:age5:hispanic + educ:race:age6:hispanic,
                 weight=marsupwt)
me <- data.frame(race=factor(1,label="White"),hispanic=factor(1,label="Non-Hispanic"),educ=20,a_age=seq(27,97))
me$age2 <- me$a_age^2
me$age3 <- me$a_age^3
me$age4 <- me$a_age^4
me$age5 <- me$a_age^5
me$age6 <- me$a_age^6
summary(fullearnings)
summary(fullincome)
maxage <- 80
##cbind(me$a_age[me$a_age<maxage],exp(predict(full3,me[me$a_age< maxage,])),exp(predict(fullt3,me[me$a_age< maxage,])),exp(predict(full4,me[me$a_age< maxage,])),exp(predict(fullt4,me[me$a_age< maxage,])),exp(predict(full5,me[me$a_age< maxage,])),exp(predict(fullt5,me[me$a_age< maxage,])),exp(predict(full6,me[me$a_age< maxage,])),exp(predict(fullt6,me[me$a_age< maxage,])))
#cbind(me$a_age[me$a_age<maxage],exp(predict(full5,me[me$a_age< maxage,])),exp(predict(fullt5,me[me$a_age< maxage,])),exp(predict(full6,me[me$a_age< maxage,])),exp(predict(fullt6,me[me$a_age< maxage,])))
save(fullearnings,fullincome,file="Model.RData")
