library(R6)
options(scipen = 999)
library(dplyr)
library(lubridate)
library(FinCal)
library(quantmod)
library(jsonlite)

addZeros <- function(number) {
  len <- nchar(number)
  zeros <- 10 - len
  fmtNumber <- paste0(strrep("0", zeros), number)
  return(fmtNumber)
}

Company <- R6Class("Company",
                   public = list(
                     ticker = NULL,
                     final  = NULL,
                     IRRs = NULL,
                     prices = NULL,
                     yearsInvesting = NULL,
                     
                     initialize = function(ticker,cik) {
                       
                       self$ticker <- ticker
                       getSymbols(ticker)
                       
                       prices <- as.data.frame(get(ticker))
                       prices$Date <- rownames(prices)
                       
                       dividends <- as.data.frame(getDividends(ticker))
                       colnames(dividends) <- "Dividends"
                       dividends$Date <- rownames(dividends)
                       
                       prices <- left_join(prices,dividends,by="Date")
                       
                       prices <- prices[,c(4,7,8)]
                       colnames(prices)[1] <- "Price"
                       prices$Dividends <- lead(prices$Dividends)
                       prices$Dividends[is.na(prices$Dividends)] <- 0
                       
                       prices$Date <- as.Date(prices$Date,format="%Y-%m-%d")
                       
                       self$prices <- prices
                       
                       prices <- prices %>% group_by(Date=floor_date(Date,"month")) %>% summarize(Price=first(Price),Dividends=sum(Dividends))
                       
                       if(nrow(prices) > 121) {
                         time <- 121
                         self$yearsInvesting <- "for 10 Years Investing"
                       } else {
                         time <- 61
                         self$yearsInvesting <- "for 5 Years Investing"
                       }
                       
                       IRR <- c()
                       IRRfinalDates  <- prices$Date[time:nrow(prices)]
                       
                       
                       for(i in c(time:nrow(prices))) {
                         prices2 <- prices[(i-time+1):i,]
                         prices2$QuantityInvested <- 1/prices2$Price
                         totalQuantities <- c()
                         q <- 0
                         for(j in prices2$QuantityInvested) {
                           q <- q + j
                           totalQuantities <- c(totalQuantities,q)
                         }
                         prices2$totalQuantities <- totalQuantities + ((totalQuantities*prices2$Dividends)/prices2$Price)
                         irr <- irr(c(rep(-1,time-1),prices2$totalQuantities[time]*prices2$Price[time]))
                         IRR <-c(IRR,irr)
                       }
                       
                       IRR <- (((1+IRR)^12)-1)*100
                       
                       IRRs <- data.frame(IRRfinalDates,IRR)
                       
                       self$IRRs <- IRRs
                      
                       
                       data <- fromJSON(readLines(paste0("./FundamentalsJSON/CIK",addZeros(cik),".json")))
                       
                       variables <- c("Assets","AssetsCurrent","LiabilitiesCurrent","StockholdersEquity",
                                      "ResearchAndDevelopmentExpense","DebtCurrent","LongTermDebt")
                       
                       final <- as.data.frame(data$facts$`us-gaap`$NetIncomeLoss$units$USD)
                       final <- final[final$fp=="FY",]
                       final$start <- as.Date(final$start,format="%Y-%m-%d")
                       final$end <- as.Date(final$end,format="%Y-%m-%d")
                       final$time <- final$end-final$start
                       final$year <- as.numeric(substring(final$start,1,4))
                       final <- final[final$time>=360,]
                       final <- final %>% group_by(year) %>% summarise_all(last)
                       final <- data.frame(final$end,final$year,final$val)
                       colnames(final) <- c("end","year","NetIncomeLoss")
                      
                       
                       
                       for (i in variables) {
                         df <- data[["facts"]][["us-gaap"]][[i]][["units"]][["USD"]]
                         df <- df[df$fp=="FY",]
                         df$end <- as.Date(df$end,format="%Y-%m-%d")
                         df <- df %>% group_by(end) %>% summarise_all(last)
                         df <- data.frame(df$end,df$val)
                         colnames(df) <- c("end",i)
                         final <- left_join(final,df,by="end")
                       }
                       
                       final$TotalDebts <- final$DebtCurrent + final$LongTermDebt
                       final$Liquidity <- final$AssetsCurrent / final$LiabilitiesCurrent
                       final$EquityOverAssets <- final$StockholdersEquity/final$Assets *100
                       final$DebtTerms <- final$DebtCurrent/final$LongTermDebt
                       
                       self$final <- final
                       },
                       
                       getPlot = function(x,y,color,title,metric,text=TRUE,typeline="o") {
                         
                         if (metric=="Billions") {
                           n=1000000000
                           lab <- "Billions of USD"
                         }
                         else if (metric=="Millions") {
                           n=1000000
                           lab <- "Millions of USD"
                         } else if (metric=="%") {
                           n=1
                           lab="%"
                         } else if (metric == "Unit"){
                           n=1
                           lab="USD"
                         }
                         plot(x,y/n,type=typeline,pch=16,
                              main=paste0(self$ticker,": ",title),
                              xlab="Years",ylab=lab,col=color,
                              ylim=c(min(y/n),max(y/n)*1.2),
                              xaxt="n",las=2)
                         axis(1,at = x, labels = x,tick=TRUE)
                         
                         if(text==TRUE) {
                           text(x,y/n,labels = round(y/n),pos=3,offset=0.5)
                         }
                         },
                         
                         getPlotLine = function(x,y,color,title,metric) {
                           
                           if (metric=="Billions") {
                             n=1000000000
                             lab <- "Billions of USD"
                           }
                           else if (metric=="Millions") {
                             n=1000000
                             lab <- "Millions of USD"
                           } else if (metric=="%") {
                             n=1
                             lab="%"
                           } else if (metric == "Unit"){
                             n=1
                             lab="USD"
                           }
                           
                           plot(x,y/n,type="l",pch=16,
                                main=paste0(self$ticker,": ",title),
                                xlab="Years",ylab=lab,col=color,
                                ylim=c(min(y/n),max(y/n)))
                       }
                   )
)

