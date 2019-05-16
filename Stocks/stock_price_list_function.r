library(tidyverse)
library(quantmod)
library(lubridate)
library(TTR)
library(RcppRoll)
library(randomForest)

stock_list<- c("FB", "NFLX")

end_date<-as.Date(today())
start_date<-as.Date("1990-01-01")

stock_price_rf<-function(ticker, st_date, e_date){
  ticker_data<-getSymbols(ticker, src="yahoo", auto.assign = FALSE, from=st_date, to=e_date)
  ticker_data<-data.frame(date=index(ticker_data), coredata(ticker_data))%>%
    rename(open=2, high=3, low=4, close=5, volume=6, adjusted=7)%>%
    mutate(
      rsi=RSI(close) 
    )
  stoch<- as.data.frame(stoch(ticker_data[,c("high", "low", "close")]))
  macd<-as.data.frame(MACD(ticker_data$close))
  wpr<-as.data.frame(WPR(ticker_data[,c("high", "low", "close")]))
  roc<-as.data.frame(ROC(ticker_data$close))
  obv<-as.data.frame(OBV(ticker_data$close, ticker_data$volume))
  
  ind<-bind_cols(ticker_data, stoch, macd, wpr, roc, obv)%>%
    rename(wpr=14, roc=15, obv=16)%>%
    ## calculate the gain for the day
    mutate(gain_loss=open-close)%>%
    
    ## calculate the percent gain/loss for the day
    mutate(percent_gain_loss=gain_loss/open*100)%>%
    
    ## 52 week calculations
    ### - calculate the 52 week high
    mutate(max52week=roll_max(high, n=252, align="right", fill=NA, na.rm=FALSE))%>%
    ### - calculate a 1 for when the 52 week high is, all other times are 0 even NAs (very important to make NAs 0 or else the count from last high will not work.)
    mutate(max52occured=as.integer(if_else(max52week==high,1,0, 0)))%>%
    ### - the difference between the 52 week max and the close
    mutate(difPerc52max=(max52week-close)/max52week)%>%
    ### - count sequentially how many days it has been since the 52 week high. 
    mutate(panel="a")%>%
    group_by(panel, idx=cumsum(max52occured==1L))%>%
    mutate(since52weekHigh=row_number())%>%
    ungroup%>%
    select(-idx)%>%
    
    ## Calculate the 52 week min (same variables were calculated for min as max)
    mutate(min52week=roll_min(low, n=252, align="right", fill=NA, na.rm=FALSE))%>%
    mutate(min52occured=as.integer(if_else(min52week==low,1,0,0)))%>%
    mutate(difPerc52min=(close-min52week)/min52week)%>%
    group_by(panel, idx=cumsum(min52occured==1L))%>%
    mutate(since52weekLow=row_number())%>%
    ungroup%>%
    select(-idx, -panel)%>%
    
    ## calculate the classifyers used for randomForests
    ### - make 15 and 30 moving windows. 
    mutate(window15=roll_sum(percent_gain_loss, n=15, align="left", fill=NA, na.rm=FALSE))%>%
    mutate(window30=roll_sum(percent_gain_loss, n=30, align="left", fill=NA, na.rm=FALSE))%>%
    
    ### - creates 4 categories for the 15 moving window analysis: 0 == if window is less than 0 (stock was negative for last 15 days), 1 == if stock went up by more than 0% but less than 10% over the last 15 days, 2 == if stock went up between 10% and 20%, and 3 == stock went up more than 30%. 
    mutate(classify15=if_else(window15<0, 0 , if_else(window15>0 & window15<10, 1, if_else(window15>10 & window15<20, 2, 3, NA_real_ ))))%>%
    ### - same as last but for 30 days
    mutate(classify30=if_else(window30<0, 0 , if_else(window30>0 & window30<10, 1, if_else(window30>10 & window30<20, 2, 3, NA_real_ ))))%>%
    
    ### - these are just easy classifyers, if stock goes up assign 1 if stock goes down classify as 0 for 15 sand 30 day windows. 
    mutate(classify15easy=if_else(window15<10, 0 , 1, NA_real_ ))%>%
    mutate(classify30easy=as.integer(if_else(window30<10, 0 , 1, NA_real_ )))%>%
    drop_na()%>%
    select(obv:slowD, since52weekHigh, difPerc52max, difPerc52min, since52weekLow, classify30easy)
  
  sampsize_min<-count(ind, as.factor(classify30easy))
  
  rf<-randomForest(as.factor(classify30easy)~ . , 
                   data=ind, 
                   ntree=1000, 
                   sampsize=c(min(sampsize_min$n), min(sampsize_min$n)), ## Need to make this happen programmitically. 
                   importance=TRUE)
  return(rf)
}

test<-stock_price_rf("FB", start_date, end_date)

test_list<- lapply(stock_list, stock_price_rf, start_date, end_date)
head(test)
View(test)

test1<-count(test, as.factor(classify30easy))
min(test1$n)
