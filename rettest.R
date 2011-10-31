library('RCurl')
library('combinat')
#q<-Sys.time()
#q
url<-paste('ftp://taxsim:02138@taxsimftp.nber.org/tmp/',format(Sys.time(),"%Y%m%d%H%M%S"),sep='')
outputurl<-paste(url,'taxsim',sep='.')
msgurl<-paste(url,'msg',sep='.')

T1 <- 60
headline<-"9 11 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 "
text<-"9 70 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
1 1989 13 3 1 0 30000 0 0 0 0 0 0 0 0 0 2000 0 1 0 -1000 0
1 1989 13 3 1 0 20000 0 0 0 0 0 0 0 0 0 2000 0 1 0 -1000 0"
income <- seq(0,500000,by=100000)
over65 <- seq(0,1)
longtermcapitalgains <- seq(0,5000000,by=1000000)
dividendincome <- seq(0,5000000,by=1000000)
dependents <- seq(0,1)
married <- seq(0,1)
socialsecurityincome <- rep(0,50000,by=10000)
#big<-expand.grid(income, over65,longtermcapitalgains,dividendincome,dependents,married,socialsecurityincome)
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
tsfile <-
    cbind(seq(1,dim(big)[1]),year,state,married,dependents,big$Var2,big$Var1,spouseincome,big$Var4,propertyincome,taxablepensions,socialsecurityincome,transferincome,rentpaid,realestatepaid,itemizeddeductions,childcareexpense,uiincome,big$Var5,nonAMTdeductions,shorttermcapgains,big$Var3)

#caseid <- seq(1,T1)
#year <- rep(2007,T1)
#state <- rep(9,T1)
#married <- rep(1,T1)
#dependents <- rep(0,T1)
#over65 <- rep(0,T1)
#over65[seq(T1-26,T1)] <- 1
#income <- rep(100000,T1)
#spouseincome <- rep(0,T1)
#dividendincome <- rep(0,T1)
#propertyincome <- rep(0,T1) 
##Propertyincome contains IRA contributions
#taxablepensions <- rep(0,T1)
#socialsecurityincome <- rep(0,T1)
#transferincome <- rep(0,T1)
#rentpaid <- rep(0,T1)
#realestatepaid <- rep(0,T1)
#itemizeddeductions <- rep(0,T1)
#childcareexpense <- rep(0,T1)
#uiincome <- rep(0,T1)
#minordependents <- rep(0,T1)
#nonAMTdeductions <- rep(0,T1)
#shorttermcapgains <- rep(0,T1)
#longtermcapgains <- rep(0,T1)
#tsfile <-
    #cbind(caseid,year,state,married,dependents,over65,income,spouseincome,dividendincome,propertyincome,taxablepensions,socialsecurityincome,transferincome,rentpaid,realestatepaid,itemizeddeductions,childcareexpense,uiincome,minordependents,nonAMTdeductions,shorttermcapgains,longtermcapgains)

temp<- file('test.txt')
write(headline,temp,append=FALSE) 
system('cat test.txt')
#writeLines(headline, con=temp)
temptable<- file('testtable.txt')
write.table(tsfile, temptable, row.names=FALSE, col.names=FALSE, append=TRUE)
system('cat testtable.txt >> test.txt')
ftpUpload('test.txt',url)
#The best way to do this for now (2011-10-30) is to take a table (tsfile)
#and then write the header line and the table iwthout row or column names to
#a file, and then upload this file.

#ftpUpload(temp,url)
#print('made it')
#ftpUpload(write.table(tsfile,textConnection()),url)
#v<-ftpUpload(textConnection(write.table(tsfile,row.names=FALSE,col.names=FALSE)),url)
#v<-ftpUpload(temp,url)

#v<-ftpUpload(I(text),url)
#v<-ftpUpload(textConnection(text),url)
#connection <- file('out.txt')
#b<-download.file(outputurl,'dl.txt')

#c<-getURL(outputurl)
#write(getURL(outputurl),connection)

names<-c('Case ID','Year','State','Federal income tax liability','State income tax liability','FICA (OADSI and HI, employee AND employer)','federal marginal rate on wage income','state marginal rate on wage income','FICA rate','Federal AGI','UI in AGI','Social Security in AGI','Zero Bracket Amount','Personal Exemptions','Exemption Phaseout','Deduction Phaseout','Deductions Allowed (Zero for non-itemizers)','Federal Taxable Income','Federal Regular Tax','Exemption Surtax','General Tax Credit','Child Tax Credit (as adjusted)','Additional Child Tax Credit (refundable)','Child Care Credit','Earned Income Credit','Income for the Alternative Minimum Tax','AMT Liability (addition to regular tax)','Federal Income Tax Before Credits','FICA','State Household Income','State Rent Payments','State AGI','State Exemption amount','State Standard Deduction','State Itemized Deductions','State Taxable Income','State Property Tax Credit','State Child Care Credit','State EIC','State Total Credits','State Bracket Rate')

#a<-read.table('out.txt')
#temp<- tempfile()
#download.file(outputurl,temp, quiet = TRUE)
#a<-read.table(temp)
#names(a)<-names
#taxsim<-read.table(textConnection(getURL(outputurl)))
#print(a)
#close(connection)
#a<-getURL(outputurl)

taxsim <- read.table(textConnection(getURL(outputurl)))
#print(getURL(msgurl))
names(taxsim)<-names
print(taxsim)
#This works to take the output file from taxsim.
save(taxsim,file="taxsim.RData")
# Save the output as a file.  It can be read back in with
# 'load('taxsim.RData')'

#a
#a<-getURL(msg)
#a


#v1 = Case ID
#v2 = Year
#v3 = State
#v4 = Federal income tax liability
#v5 = State income tax liability
#v6 = FICA (OADSI and HI, employee AND employer)
#v7 = federal marginal rate on wage income
#v8 = state marginal rate on wage income
#v9 = FICA rate
#v10 = Federal AGI
#v11 = UI in AGI
#v12 = Social Security in AGI
#v13 = Zero Bracket Amount
#v14 = Personal Exemptions
#v15 = Exemption Phaseout
#v16 = Deduction Phaseout
#v17 = Deductions Allowed (Zero for non-itemizers)
#v18 = Federal Taxable Income
#v19 = Federal Regular Tax
#v20 = Exemption Surtax
#v21 = General Tax Credit
#v22 = Child Tax Credit (as adjusted)
#v23 = Additional Child Tax Credit (refundable)
#v24 = Child Care Credit
#v25 = Earned Income Credit
#v26 = Income for the Alternative Minimum Tax
#v27 = AMT Liability (addition to regular tax)
#v28 = Federal Income Tax Before Credits
#v29 = FICA
#v30 = State Household Income
#v31 = State Rent Payments
#v32 = State AGI
#v33 = State Exemption amount
#v34 = State Standard Deduction
#v35 = State Itemized Deductions
#v36 = State Taxable Income
#v37 = State Property Tax Credit
#v38 = State Child Care Credit
#v39 = State EIC
#v40 = State Total Credits
#v41 = State Bracket Rate

#State codes:
#1Alabama
#2Alaska
#3Arizona
#4Arkansas
#5California
#6Colorado
#7Connecticut
#8Delaware
#9DC
#10Florida
#11Georgia
#12Hawaii
#13Idaho
#14Illinois
#15Indiana
#16Iowa
#17Kansas
#18Kentucky
#19ouisiana
#20AlaskaMaine
#21Maryland
#22Massachusetts
#23Michigan
#24Minnesota
#25Mississippi
#26Missouri
#27Montana
#28Nebraska
#29Nevada
#30NewHampshire
#31NewJersey
#32NewMexico
#33NewYork
#34NorthCarolina
#35NorthDakota
#36Ohio
#37Oklahoma
#38Oregon
#39Pennsylvania
#40RhodeIsland
#41SouthCarolina
#42SouthDakota
#43Tennessee
#44Texas
#45Utah
#46Vermont
#47Virginia
#48Washington
#49WestVirginia
#50CaliforniaWisconsin
#51Wyoming

#Input parameters:

#1. Case ID (arbitrary, but must be a non-negative numeric)
#2. Tax year (4 digits between 1960 and 2013, but state must be zero if year is before 1977 or after 2008. We don't have code for state laws before 1977.) Indexed tax parameters are inflated by 2.5%/year after 2007.
#3. State (SOI codes. These run from 1 for Alabama to 51 for Wyoming and are not the Census or PSID codes. See state list,and also item two above.). Use zero for "no state tax calculation".
#4. Marital Status (1. single 2. joint 3. head of household 8. Dependent taxpayer )
#5. Dependent Exemptions (including children of all ages, but see #19 below)
#6. Number of taxpayers over 65 years of age.
#7. Wage and salary income of Taxpayer (include self-employment).
#8. Wage and salary income of Spouse (include self-employment).
#9. Dividend income (qualified dividends only for 2003 on).
#10. Other property income, including interest rent alimony fellowships non-qualified dividends state income tax refunds (itemizers only) taxable IRA distributions capital gains distributions on form 1040 other income or loss not otherwise enumerated here Adjustments and items such as alimony paid Keogh and IRA contributions foreign income exclusion NOLs can be entered here as negative income.(+/-)
#11. Taxable Pensions
#12. Gross Social Security Benefits
#13. Other non-taxable transfer Income such as welfare, and child support that would affect eligibility for state property tax rebates but would not be taxable at the federal level.
#14. Rent Paid (used only for calculating state property tax rebates)
#15. Real Estate taxes paid. This is a preference for the AMT and is is also used to calculate state property tax rebates.
#16. Itemized deductions that are a preference for the Alternative Minimum Tax. These would include Other state and local taxes (line 8 of Schedule A) plus local income tax Deductible Medial Expenses up to 10% of AGI (line 4) (7.5% pre 1990) Miscellaneous (line 27) Obviously most of these will not be available in the typical survey, which will affect the accuracy of the tax calculation but won't disturb the software. Do not include real estate taxes or state income tax as they will be calculated from other data on this page. A state sales tax deduction will be calculated according to publication 600. If items 16, 20 and our calculation of state income tax do not exceed the standard deduction, then the taxpayer will be given the standard deduction instead.
#17. Child care expenses.
#18. Unemployment compensation received.
#19. Number of dependents under age 17 (for child credit, not more than item 5).  
#20. Deductions not included in item 16 and not a preference for the AMT, including (on Schedule A for 2009) Deductible medical expenses in excess of 10% of AGI (only 1990+) Motor Vehicle Taxes paid (line 7 of schedule A) Home mortgage interest (line 15) Charitable contributions (line 19) Casulty or Theft Losses (line 20) 
#21. Short Term Capital Gains or losses.  (+/-) 
#22. Long Term Capital Gains or losses.  (+/-)




#1. Case ID 
#2. Tax year
#3. State 
#4. Marital Status
#5. Dependent Exemptions
#6. Number of taxpayers over 65 years of age.
#7. Wage and salary income of Taxpayer (include self-employment).
#8. Wage and salary income of Spouse (include self-employment).
#9. Dividend income (qualified dividends only for 2003 on).
#10. Other property income, including interest rent alimony fellowships non-qualified dividends state income tax refunds (itemizers only) taxable IRA distributions capital gains distributions on form 1040 other income or loss not otherwise enumerated here Adjustments and items such as alimony paid Keogh and IRA contributions foreign income exclusion NOLs can be entered here as negative income.(+/-)
#11. Taxable Pensions
#12. Gross Social Security Benefits
#13. Other non-taxable transfer Income such as welfare, and child support that would affect eligibility for state property tax rebates but would not be taxable at the federal level.
#14. Rent Paid (used only for calculating state property tax rebates)
#15. Real Estate taxes paid. This is a preference for the AMT and is is also used to calculate state property tax rebates.
#16. Itemized deductions that are a preference for the Alternative Minimum Tax. These would include Other state and local taxes (line 8 of Schedule A) plus local income tax Deductible Medial Expenses up to 10% of AGI (line 4) (7.5% pre 1990) Miscellaneous (line 27) Obviously most of these will not be available in the typical survey, which will affect the accuracy of the tax calculation but won't disturb the software. Do not include real estate taxes or state income tax as they will be calculated from other data on this page. A state sales tax deduction will be calculated according to publication 600. If items 16, 20 and our calculation of state income tax do not exceed the standard deduction, then the taxpayer will be given the standard deduction instead.
#17. Child care expenses.
#18. Unemployment compensation received.
#19. Number of dependents under age 17 (for child credit, not more than item 5).  
#20. Deductions not included in item 16 and not a preference for the AMT, including (on Schedule A for 2009) Deductible medical expenses in excess of 10% of AGI (only 1990+) Motor Vehicle Taxes paid (line 7 of schedule A) Home mortgage interest (line 15) Charitable contributions (line 19) Casulty or Theft Losses (line 20) 
#21. Short Term Capital Gains or losses.  (+/-) 
#22. Long Term Capital Gains or losses.  (+/-)
