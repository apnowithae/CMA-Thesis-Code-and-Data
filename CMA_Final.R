
#2015 Calculation

library(pracma)
library(rootSolve)
library(pracma)
library(readxl)

RCMAM <- as.matrix(read_excel('95x95 Datasets.xlsx', sheet = 2, range = 'E200:CU294', col_names = FALSE))
FCMAM <- as.matrix(read_excel('95x95 Datasets.xlsx', sheet = 2, range = 'E297:CU391', col_names = FALSE))

INVVAR <- function(x) {c(1/x[i], i <- 1:190)}

ZVEC <- as.matrix(c(rep(0,95)))
ZMAT <- cbind(rep(ZVEC, 95)) #Zero matrix

Top <- cbind(ZMAT, RCMAM)
Bot <- cbind(FCMAM, ZMAT)
DES_MAT <- rbind(Top, Bot) #Final 190x190 design matrix

FUN_VEC <- function(x) {DES_MAT%*%INVVAR} #This gives the RHS summation of the CMA equations. 190 equations total
FUN <- function(x) {FUN_VEC - c(x[i], i <- 1:190)}

CMA_Vec_2015 <- newton2(FUN, c(rep(1,190)), 100) #100 iterations for solution


options(scipen = 999) #Display as decimal not in scientific notation
Log_15 <- log(abs(CMA_Vec_2015))

#2000 Calculations

RCMAM <- as.matrix(read_excel('95x95 Datasets.xlsx', sheet = 1, range = 'E200:CU294', col_names = FALSE))
FCMAM <- as.matrix(read_excel('95x95 Datasets.xlsx', sheet = 1, range = 'E297:CU391', col_names = FALSE))

INVVAR <- function(x) {c(1/x[i], i <- 1:190)}

ZVEC <- as.matrix(c(rep(0,95)))
ZMAT <- cbind(rep(ZVEC, 95)) #Zero matrix

Top <- cbind(ZMAT, RCMAM)
Bot <- cbind(FCMAM, ZMAT)
DES_MAT <- rbind(Top, Bot) #Final 190x190 design matrix

FUN_VEC <- function(x) {DES_MAT%*%INVVAR} #This gives the RHS summation of the CMA equations. 190 equations total
FUN <- function(x) {FUN_VEC - c(x[i], i <- 1:190)} #REDO

CMA_Vec_2000 <- newton2(FUN, c(rep(1,190)), 100) #100 iterations for solution


options(scipen = 999)
Log_00 <- log(abs(CMA_Vec_2000))

plot(CMA_Vec_2000, CMA_Vec_2015)

#Note: 2-64 are MD, 65-95 are VA
#Parse the data as such

#DEFINING THE PLACES
place_names <- as.matrix(read_excel('95x95 Datasets.xlsx', sheet = 1, range = 'A3:A97', col_names = FALSE))
CMA_Det_2015_res <- cbind(place_names, CMA_Vec_2015[1:95])
CMA_Det_2015_off <- cbind(place_names, CMA_Vec_2015[96:190])
CMA_Det_2000_res <- cbind(place_names, CMA_Vec_2000[1:95])
CMA_Det_2000_off <- cbind(place_names, CMA_Vec_2000[96:190])



#RCMA ANALYSIS WITH HOME PRICES
HomePrices_2000 <- as.matrix(read_excel('95x95 Datasets.xlsx', sheet = 5, range = 'b2:b96', col_names = FALSE))
HomePrices_2015 <- as.matrix(read_excel('95x95 Datasets.xlsx', sheet = 5, range = 'c2:c96', col_names = FALSE))

#Simple Regression with only RCMA Change
ln_HP_Change <- ln_HP_change
plot(ln_RCMA_change, ln_HP_Change, main = "ln Home Val Change vs ln RCMA Change", 
     xlab = 'Change in ln RCMA', ylab = 'Change in ln ZHVI')
summary(lm(ln_HP_Change~ln_RCMA_change))

#Full Regression
SFR_2015 <- as.matrix(read_excel('95x95 Datasets.xlsx', sheet = 6, range = 'c2:c96', col_names = FALSE))
SFR_2000 <- as.matrix(read_excel('95x95 Datasets.xlsx', sheet = 6, range = 'b2:b96', col_names = FALSE))
ln_SFR_change <- log(SFR_2015) - log(SFR_2000)
fit1 <- lm(ln_HP_Change~ln_SFR_Change+ln_RCMA_change+ln_SFR_Change:ln_RCMA_change) #Not much interaction
summary(fit1)

#Direct fit between SFR and HP
hp_sfr <- lm(ln_HP_Change~ln_SFR_Change)
par(mfrow = c(1,2))
summary(hp_sfr)
plot(ln_HP_Change~ln_SFR_Change, main = 'Change in ln ZHVI versus Change in ln SFR',
     xlab = 'log ratio of 2015 to 2000 SFR', ylab = 'log ratio of 2015 to 2000 ZHVI')
'plot(hp_sfr, main = "Residuals versus fitted")'
plot(hp_sfr, main = 'Residuals v. Fitted')

#Comparing RCMA and FCMA across 2000 and 2015
RCMA_Compare <- cbind(place_names, CMA_Vec_2015[1:95], CMA_Vec_2000[1:95])
FCMA_Compare <- cbind(place_names, CMA_Vec_2015[96:190], CMA_Vec_2000[96:190])


#FCMA ANALYSIS WITH PWGDP

#Getting PWGDP Data from Data Matrix
PWGDP_2000 <- as.matrix(read_excel('95x95 Datasets.xlsx', sheet = 7, range = 'b2:b96', col_names = FALSE))
PWGDP_2015 <- as.matrix(read_excel('95x95 Datasets.xlsx', sheet = 7, range = 'c2:c96', col_names = FALSE))
PWGDP_2000

#PWGDP vs only FCMA
ln_PWGDP_chg <- log(PWGDP_2015/PWGDP_2000)
ln_FCMA_change <- log(CMA_Vec_2015[96:190]/CMA_Vec_2000[96:190])

plot(ln_FCMA_change, ln_PWGDP_change, main = "ln GDP per Worker Change v. ln FCMA Change")
summary(lm(ln_PWGDP_change~ln_FCMA_change))

#Employment Data
emp_2015 <- as.matrix(read_excel('95x95 Datasets.xlsx', sheet = 2, range = 'b100:b194', col_names = FALSE))
emp_2000 <- as.matrix(read_excel('95x95 Datasets.xlsx', sheet = 1, range = 'b100:b194', col_names = FALSE))

#Regression with only PWGDP and employment change, i.e. no FCMA
ln_emp_Change <- log(emp_2015/emp_2000)
plot(ln_PWGDP_change~ln_emp_Change)
plot(ln_PWGDP_change~ln_emp_Change, col=ifelse(ln_emp_Change>=2,"red","black"), 
     main = 'log PWGDP Change Ratio v. log Emp. Change Ratio',
     xlab = 'log Emp. Change Ratio', ylab = 'log PWGDP Change Ratio')

summary(lm(ln_PWGDP_change~ln_emp_Change))


#Removing the outlier (index 59 Springdale MD)
ln_PWGDP_mod <- ln_PWGDP_change[c(1:58, 60:95)]
ln_emp_mod <- ln_emp_Change[c(1:58, 60:95)]
ln_FCMA_mod <- ln_FCMA_change[c(1:58,60:95)]

#Reperforming the regression
par(mfrow = c(1,2))
plot(ln_PWGDP_mod~ln_emp_mod, 
     main = 'Updated log PWGDP Change Ratio v. log Emp. Change Ratio',
     xlab = 'log Emp. Change Ratio', ylab = 'log PWGDP Change Ratio')
plot(lm(ln_PWGDP_mod~ln_emp_mod), main = 'Residuals v. Fitted')
summary(lm(ln_PWGDP_mod~ln_emp_mod))

#Full regression analysis
summary(lm(ln_PWGDP_mod~ln_emp_mod + ln_FCMA_mod + ln_emp_mod:ln_FCMA_mod))

#Plotting solely relationship with FCMA
summary(lm(ln_PWGDP_mod~ln_FCMA_mod))
par(mfrow = c(1,2))
plot(ln_PWGDP_mod~ln_FCMA_mod, 
     main = 'log PWGDP Change Ratio v. log FCMA Ratio',
     xlab = 'log FCMA Ratio', ylab = 'log PWGDP Change Ratio')
plot(lm(ln_PWGDP_mod~ln_emp_mod), main = 'Residuals v. Fitted')

RCMA_Compare <- cbind(place_names, CMA_Vec_2015[1:95], CMA_Vec_2000[1:95])
FCMA_Compare <- cbind(place_names, CMA_Vec_2015[96:190], CMA_Vec_2000[96:190])
        