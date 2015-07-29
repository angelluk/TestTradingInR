library(quantmod)
library(PerformanceAnalytics)

# read data
s <- get(getSymbols('AAPL',src="google"))["1996::"]

adjustOHLC(s,
           adjust = c("split","dividend"),
           use.Adjusted = FALSE,
           ratio = NULL,
           symbol.name=deparse(substitute(x)))
# choose Close
Rub= s[,4]
head(Rub)
tail(Rub)

Rub$Cl = Cl(Rub)
# percent of Close
Rub$perRub =  dailyReturn(Rub$Cl)

# separate set for %
cRub = Rub$perRub 
summary(cRub)


sig = IQR(cRub)

# tarding rules
cRub$position <- 0 #ifelse(Lag(cRub,1)> 0 & Lag(cRub,2)>0 , 1, 0)

buyOpen <-0
buyClose <-0

for (n in 1:length (cRub)-1){  
  
  if (n>3){

  # bay open signal if chain down 1 then buy
  if (cRub[n-1,1] <0 ) { 
    
    #if (cRub[n-2,1] >0 ){
      buyOpen <- 1
    #  }

  }
  
  # bay close signal if chain up 2 then sell
  if (cRub[n-1,1] > 0 ) {
    if (cRub[n-2,1] > 0 ){
     # if (cRub[n-3,1] < 0 ){
        buyOpen <- 0
     #}
    }
  }
  
  if(buyOpen == 1) 
    cRub[n,2] <- 1
  
  if(buyOpen == 0) 
    cRub[n,2] <- 0
  }
}

head(cRub)
tail(cRub)
# check how offen appears the signal 
table(cRub$position)
# return og system
cRub$ret <- cRub$perRub * cRub$position

# PLOT
require(PerformanceAnalytics)
# create table showing drawdown statistics
table.Drawdowns(cRub$ret, top=10)
# create table of downside risk estimates
table.DownsideRisk(cRub$ret)
# chart equity curve, daily performance, and drawdowns
charts.PerformanceSummary(cbind(Rub$perRub ,cRub$ret))
